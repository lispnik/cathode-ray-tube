//  crt.metal -- every shader in cathode-ray-tube, in one file.
//
//  Ported from cool-retro-term's GLSL (Filippo Scognamiglio, GPL-3).  See
//  CREDITS.md; this file is a translation of app/shaders/*.frag and inherits
//  their licence.
//
//  ONE SOURCE, NOT FIFTY-SIX.  Upstream's two big fragment shaders are
//  ubershaders guarded by #ifdefs, and its build bakes every combination into a
//  separate .qsb -- forty of terminal_dynamic and sixteen of terminal_static --
//  which the application then selects by concatenating a filename.  Metal has
//  function constants, so the same specialisation happens when the function is
//  created: the compiler folds the constants and strips the dead branches, and
//  what runs is what the baked variant contained.  The variant set stops being
//  a build artifact and becomes a tuple in a hash table.
//
//  Constant indices are mirrored in src/metal/constants.lisp's callers; keep
//  them in step.

#include <metal_stdlib>
using namespace metal;

// ---------------------------------------------------------------------------
// Function constants.  Indices 0-7, mirroring upstream's four-plus-four #ifdefs.
// ---------------------------------------------------------------------------

constant int  CRT_RASTER_MODE     [[function_constant(0)]];  // 0..4
constant bool CRT_BURN_IN         [[function_constant(1)]];
constant bool CRT_DISPLAY_FRAME   [[function_constant(2)]];
constant bool CRT_CHROMA          [[function_constant(3)]];
constant bool CRT_RGB_SHIFT       [[function_constant(4)]];
constant bool CRT_BLOOM           [[function_constant(5)]];
constant bool CRT_CURVATURE       [[function_constant(6)]];
constant bool CRT_FRAME_SHININESS [[function_constant(7)]];

// ---------------------------------------------------------------------------
// The fullscreen quad.
//
//  No vertex buffer anywhere in this program: four vertices synthesised from
//  [[vertex_id]] as a triangle strip.  vid 0..3 gives (0,0) (1,0) (0,1) (1,1)
//  in UV, mapped to clip space.
//
//  uv.y runs 0 at the BOTTOM to 1 at the top, matching GLSL's convention and
//  therefore matching every ported shader.  Metal's texture origin is top-left,
//  so a readback's row 0 is uv.y = 1 -- which is only ever visible in a test
//  that asserts on a particular pixel, and is noted there.
// ---------------------------------------------------------------------------

struct Varyings {
    float4 position [[position]];
    float2 uv;
};

vertex Varyings fullscreen_vertex(uint vid [[vertex_id]])
{
    float2 uv = float2(float(vid & 1u), float(vid >> 1u));
    Varyings out;
    out.position = float4(uv * 2.0 - 1.0, 0.0, 1.0);
    out.uv = uv;
    return out;
}

// ---------------------------------------------------------------------------
// Shared helpers, ported verbatim from upstream.
// ---------------------------------------------------------------------------

//  The luminance weights upstream uses.  NOT Rec. 709 and not Rec. 601 -- these
//  are the rounded numbers in terminal_dynamic.frag and burn_in.frag, and the
//  burn-in mask compares two of these against each other, so the rounding has
//  to match or the mask flickers at the threshold.
inline float rgb2grey(float3 v) { return dot(v, float3(0.21, 0.72, 0.04)); }

//  The classic sin-hash.  It will NOT match upstream bit for bit on an Apple
//  GPU, because sin() is computed differently -- but it feeds a +/-0.0125
//  dither and a +/-0.02 grain, so the difference is invisible.  Keep the
//  formula for faithfulness; never assert on anything downstream of it.
inline float rand2(float2 v)
{
    return fract(sin(dot(v, float2(12.9898, 78.233))) * 43758.5453);
}

// ---------------------------------------------------------------------------
// The text pass.
//
//  One instanced draw covers the whole screen: a quad per cell background and a
//  quad per glyph, in that order, with straight-alpha blending so the glyphs
//  composite over the backgrounds.  Both sample the SAME atlas -- backgrounds
//  read a 2x2 solid block reserved at its origin -- so there is one pipeline,
//  one texture binding and one draw call for the entire terminal.
//
//  The output of this pass is the texture every effect in the chain consumes.
//  Two things about it are load-bearing and easy to lose:
//
//    It is cleared to (0,0,0,0) and the PROFILE BACKGROUND IS NOT PAINTED HERE.
//    terminal_dynamic.frag's convertWithChroma computes
//    mix(backgroundColor, foregroundColor, rgb2grey(inColor)) -- the background
//    is applied in the DYNAMIC pass.  Painting it here too would apply it twice
//    and break every one of the fourteen profiles.
//
//    Alpha is a MASK.  Cells inside the content rectangle write alpha 1 and the
//    margin ring stays 0, because the static pass uses the BLURRED alpha as its
//    bloom mask and its frame-reflection mask.  Losing it is silent and shows up
//    as "bloom looks wrong".
// ---------------------------------------------------------------------------

struct CellInstance {
    float4 color;     // straight alpha
    float2 origin;    // top-left, in texture pixels
    float2 size;      // in texture pixels
    float2 uv0;
    float2 uv1;
};

struct TextUniforms {
    float2 target_size;   // the text target, in pixels
};

struct TextVaryings {
    float4 position [[position]];
    float2 uv;
    float4 color;
};

vertex TextVaryings text_vertex(uint vid [[vertex_id]],
                                uint iid [[instance_id]],
                                const device CellInstance *cells [[buffer(0)]],
                                constant TextUniforms &u [[buffer(1)]])
{
    CellInstance cell = cells[iid];
    float2 corner = float2(float(vid & 1u), float(vid >> 1u));

    float2 pixel = cell.origin + corner * cell.size;
    // Pixels to clip space.  Y is flipped because the cell grid counts rows
    // DOWNWARD from the top, the way a terminal does, while clip space counts
    // upward.
    float2 ndc = float2((pixel.x / u.target_size.x) * 2.0 - 1.0,
                        1.0 - (pixel.y / u.target_size.y) * 2.0);

    TextVaryings out;
    out.position = float4(ndc, 0.0, 1.0);
    out.uv = mix(cell.uv0, cell.uv1, corner);
    out.color = cell.color;
    return out;
}

fragment float4 text_fragment(TextVaryings in [[stage_in]],
                              texture2d<float> atlas [[texture(0)]],
                              sampler atlas_sampler [[sampler(0)]])
{
    // The atlas is R8Unorm coverage, so only .r carries anything.
    float coverage = atlas.sample(atlas_sampler, in.uv).r;
    float alpha = in.color.a * coverage;
    // Premultiplied would be wrong here: the blend state is straight-alpha
    // source-over, so the colour is handed over unmultiplied and the hardware
    // does the multiply.
    return float4(in.color.rgb, alpha);
}

// ---------------------------------------------------------------------------
// Blit.
//
//  Copies a texture to the target, one to one.  It is what the window draws
//  until the effect chain lands in M3, and it stays afterwards as the honest
//  "effects off" path -- so it is not scaffolding.
// ---------------------------------------------------------------------------

fragment float4 blit_fragment(Varyings in [[stage_in]],
                              texture2d<float> source [[texture(0)]],
                              sampler source_sampler [[sampler(0)]])
{
    // The quad's uv.y runs bottom-to-top and the text target is stored
    // top-down, so the sample is flipped.  Getting this wrong renders the
    // terminal upside down, which is at least an unambiguous symptom.
    float2 uv = float2(in.uv.x, 1.0 - in.uv.y);
    float4 texel = source.sample(source_sampler, uv);
    // The text target's alpha is a MASK for the effect chain, not transparency:
    // the window is opaque, so it is dropped here.
    return float4(texel.rgb, 1.0);
}

// ===========================================================================
// THE EFFECT CHAIN
//
//  Ported from cool-retro-term's terminal_frame.frag, terminal_static.frag,
//  burn_in.frag and terminal_dynamic.{vert,frag}.  The maths is theirs; what is
//  different here is that the four #ifdef'd ubershaders are one source
//  specialised by function constants instead of fifty-six baked files.
//
//  The chain is:  T -> static -> S -> dynamic -> screen
//  with burn-in and bloom branching off T directly, and the bezel rendered once
//  into F.  (ShaderTerminal.qml: staticShader.source is the terminal source,
//  frameBuffer wraps staticShader, dynamicShader.screenBuffer is frameBuffer.)
// ===========================================================================

//  The barrel distortion, and it appears in THREE passes with three different
//  consequences.  The static pass warps the text and defines the reflection
//  mask; the frame pass pre-warps the bezel so the two line up; the dynamic
//  pass uses it ONLY for the burn-in lookup and the rasterisation grid -- never
//  for the main text sample, which is already warped.
//
//  That last point is what makes jitter read as the beam wobbling behind a
//  fixed grille: the scanlines follow the curvature but not the jitter.
inline float2 distort_coordinates(float2 coords, float frame_size, float curvature)
{
    float2 padded = coords * (1.0 + frame_size * 2.0) - frame_size;
    float2 cc = padded - float2(0.5);
    float dist = dot(cc, cc) * curvature;
    return padded + cc * (1.0 + dist) * dist;
}

// ---------------------------------------------------------------------------
// The bezel.  Fully procedural: no textures at all.
//
//  Older cool-retro-term shipped frames/*.png; this version draws a rounded
//  rectangle with a signed distance field, four mitred bevel quadrants split
//  along the diagonals of the unit square, and a hash grain.
// ---------------------------------------------------------------------------

struct FrameUniforms {
    float4 frame_color;
    float2 viewport_size;
    float  screen_curvature;
    float  frame_size;
    float  screen_radius;     // PIXELS, not a fraction
    float  ambient_light;
    float  frame_shininess;
    float  opacity;
};

inline float rounded_rect_sdf(float2 p, float2 top_left, float2 bottom_right,
                              float radius_pixels, float2 viewport)
{
    float2 size = (bottom_right - top_left) * viewport;
    float2 centre = (top_left + bottom_right) * 0.5 * viewport;
    float2 local = p * viewport - centre;
    float2 half_size = size * 0.5 - float2(radius_pixels);
    float2 d = abs(local) - half_size;
    return length(max(d, float2(0.0))) + min(max(d.x, d.y), 0.0) - radius_pixels;
}

fragment float4 frame_fragment(Varyings in [[stage_in]],
                               constant FrameUniforms &u [[buffer(0)]])
{
    float2 static_coords = in.uv;
    float2 coords = distort_coordinates(static_coords, u.frame_size, u.screen_curvature);

    float edge_soft = 1.0;
    float seam = max(u.screen_radius, 0.5) / min(u.viewport_size.x, u.viewport_size.y);

    // The four bevel quadrants, split along both diagonals of the unit square.
    float e = min(smoothstep(-seam, seam, coords.x - coords.y),
                  smoothstep(-seam, seam, coords.x - (1.0 - coords.y)));
    float s = min(smoothstep(-seam, seam, coords.y - coords.x),
                  smoothstep(-seam, seam, coords.x - (1.0 - coords.y)));
    float w = min(smoothstep(-seam, seam, coords.y - coords.x),
                  smoothstep(-seam, seam, (1.0 - coords.x) - coords.y));
    float n = min(smoothstep(-seam, seam, coords.x - coords.y),
                  smoothstep(-seam, seam, (1.0 - coords.x) - coords.y));

    float dist = rounded_rect_sdf(coords, float2(0.0), float2(1.0),
                                  u.screen_radius, u.viewport_size);
    // 0.66 / 1.0 / 0.66 / 0.33: a light source low and in front, so the bottom
    // edge is brightest and the top is darkest.
    float shadow = (e * 0.66 + w * 0.66 + n * 0.33 + s);
    shadow *= smoothstep(0.0, edge_soft * 5.0, dist);

    float frame_alpha = 1.0 - u.frame_shininess * 0.4;
    float in_screen = smoothstep(0.0, edge_soft, -dist);
    float alpha = mix(frame_alpha, mix(0.0, 0.3, u.ambient_light), in_screen);

    // The sheen of room light on the glass: brightest at the centre, falling
    // off toward the edges.
    float2 g = coords * (1.0 - coords.yx);
    float glass = clamp(u.ambient_light * pow(g.x * g.y * 25.0, 0.5) * in_screen,
                        0.0, 1.0);

    float3 tint = u.frame_color.rgb * shadow;
    float noise = rand2(static_coords * u.viewport_size) - 0.5;
    tint = clamp(tint + float3(noise * 0.04), 0.0, 1.0);

    float3 colour = mix(tint, float3(glass), in_screen);
    // PREMULTIPLIED, unlike the dynamic pass.  The dynamic pass composites this
    // with mix(colour, frame.rgb, frame.a), which wants the colour unmultiplied
    // -- so the multiply by opacity here is upstream's `* qt_Opacity' on the
    // whole vec4 and nothing more.
    return float4(colour, alpha) * u.opacity;
}

// ---------------------------------------------------------------------------
// Burn-in: the phosphor accumulator.
//
//  ALPHA IS A LIT MASK, not transparency.  Where a pixel is currently lit, decay
//  is suppressed for this step and the dynamic pass zeroes the burn-in
//  contribution -- otherwise live glyphs would add their own trail on top of
//  themselves and bloom into a smear.
//
//  Qt reads this target recursively; Metal forbids that, so the Lisp side
//  ping-pongs two textures.  It is NUMERICALLY IDENTICAL, not an approximation:
//  the shader only ever reads the texel it writes, with no neighbourhood taps.
// ---------------------------------------------------------------------------

struct BurnInUniforms {
    float last_update;
    float previous_update;
    float burn_in_time;    // a RATE: 1 / lint(0.16, 1.6, burnIn)
    float opacity;
};

fragment float4 burn_in_fragment(Varyings in [[stage_in]],
                                 constant BurnInUniforms &u [[buffer(0)]],
                                 texture2d<float> source [[texture(0)]],
                                 texture2d<float> previous [[texture(1)]],
                                 sampler smp [[sampler(0)]])
{
    float2 coords = float2(in.uv.x, 1.0 - in.uv.y);
    float3 txt = source.sample(smp, coords).rgb;
    float4 acc = previous.sample(smp, coords);

    float prev_mask = acc.a;
    float decay = clamp((u.last_update - u.previous_update) * u.burn_in_time, 0.0, 1.0);
    decay = max(0.0, decay - prev_mask);

    float3 colour = max(acc.rgb - float3(decay), txt);
    float curr_mask = step(rgb2grey(colour), rgb2grey(txt));
    return float4(colour, curr_mask) * u.opacity;
}

// ---------------------------------------------------------------------------
// Bloom: a separable Gaussian.
//
//  Upstream uses Qt's FastBlur, which has no source in the repository and is an
//  undisclosed multi-level box approximation.  This is a two-pass Gaussian, so
//  the result is CLOSE rather than identical -- the one place in the chain where
//  exact parity is not available at any price.
//
//  There is NO BRIGHT PASS and there must not be: FastBlur blurs the raw
//  terminal texture with no threshold, and the blurred ALPHA is what the static
//  pass uses as its bloom mask.  Adding an extraction step here would change
//  both the colour and the mask.
// ---------------------------------------------------------------------------

struct BlurUniforms {
    float2 texel;       // 1 / source size, times the axis being blurred
    float  radius;
};

fragment float4 blur_fragment(Varyings in [[stage_in]],
                              constant BlurUniforms &u [[buffer(0)]],
                              texture2d<float> source [[texture(0)]],
                              sampler smp [[sampler(0)]])
{
    float2 coords = float2(in.uv.x, 1.0 - in.uv.y);
    // Nine taps with linear-sampling pairs would be faster; nine plain taps are
    // easier to be sure about, and this runs at half resolution on a handful of
    // megapixels.
    const float weights[5] = {0.2270270270, 0.1945945946, 0.1216216216,
                              0.0540540541, 0.0162162162};
    float4 sum = source.sample(smp, coords) * weights[0];
    for (int i = 1; i < 5; i++) {
        float2 offset = u.texel * (float(i) * u.radius);
        sum += source.sample(smp, coords + offset) * weights[i];
        sum += source.sample(smp, coords - offset) * weights[i];
    }
    return sum;
}

// ---------------------------------------------------------------------------
// The static pass: curvature, RGB shift, bloom, the bezel reflection, dither.
// ---------------------------------------------------------------------------

struct StaticUniforms {
    float screen_curvature;
    float rgb_shift;
    float frame_shininess;
    float frame_size;
    float screen_brightness;
    float bloom;
    float opacity;
};

fragment float4 static_fragment(Varyings in [[stage_in]],
                                constant StaticUniforms &u [[buffer(0)]],
                                texture2d<float> source [[texture(0)]],
                                texture2d<float> bloom_source [[texture(1)]],
                                sampler smp [[sampler(0)]])
{
    float2 uv = float2(in.uv.x, 1.0 - in.uv.y);

    float shown = 1.0;
    float is_reflection = 0.0;
    float is_screen = 1.0;
    float2 txt_coords = uv;

    if (CRT_CURVATURE) {
        float2 curved = distort_coordinates(uv, u.frame_size, u.screen_curvature);
        float2 inside = step(float2(0.0), curved) - step(float2(1.0), curved);
        shown = max(inside.x, inside.y);
        is_screen = min(inside.x, inside.y);
        is_reflection = shown - is_screen;
        // The MIRROR trick.  Outside [0,1] the coordinate is reflected back --
        // -c below zero, 2-c above one -- which is what smears the screen's
        // light across the bezel as a reflection instead of clamping it to a
        // stripe of edge pixels.
        txt_coords = curved * (-1.0 + 2.0 * step(float2(0.0), curved)
                                    - 2.0 * step(float2(1.0), curved));
    }

    float3 txt = source.sample(smp, txt_coords).rgb;

    if (CRT_RGB_SHIFT) {
        float2 displacement = float2(u.rgb_shift, 0.0);
        float3 right = source.sample(smp, txt_coords + displacement).rgb;
        float3 left  = source.sample(smp, txt_coords - displacement).rgb;
        // Asymmetric on purpose: red leans right, blue leans left, so the
        // fringing has a direction rather than just blurring.
        txt.r = left.r * 0.10 + right.r * 0.30 + txt.r * 0.60;
        txt.g = left.g * 0.20 + right.g * 0.20 + txt.g * 0.60;
        txt.b = left.b * 0.30 + right.b * 0.10 + txt.b * 0.60;
    }

    float3 final = txt * shown;

    float3 bloom_colour = txt;
    float bloom_alpha = 0.0;
    if (CRT_BLOOM || CRT_FRAME_SHININESS) {
        float4 full = bloom_source.sample(smp, txt_coords);
        bloom_colour = full.rgb;
        bloom_alpha = full.a;
    }

    if (CRT_BLOOM) {
        float3 on_screen = bloom_colour * is_screen;
        final += clamp(on_screen * u.bloom * bloom_alpha, 0.0, 0.5);
        // The HEADROOM PAIR.  This divide and the matching multiply in the
        // dynamic pass exist because Qt's intermediate framebuffer is 8-bit
        // UNORM and the added bloom would clip.  Keeping both is what makes the
        // highlights match; making this target RGBA16Float and dropping the
        // pair is the better renderer and a visibly brighter picture.
        final /= (1.0 + max(u.bloom, 0.0));
    }

    if (CRT_FRAME_SHININESS) {
        float3 reflection = mix(bloom_colour * bloom_alpha * 2.0, final,
                                u.frame_shininess * 0.5);
        final = mix(final, reflection, is_reflection);
    }

    final *= u.screen_brightness;

    // A frozen spatial dither, not animated: it hides banding in the gradients
    // without adding a second source of shimmer on top of the static noise.
    float noise = rand2(in.uv) - 0.5;
    final = clamp(final + float3(noise * 0.025), 0.0, 1.0);
    return float4(final, u.opacity);
}

// ---------------------------------------------------------------------------
// The dynamic pass: everything that moves, and the composite.
// ---------------------------------------------------------------------------

struct DynamicUniforms {
    float4 font_color;
    float4 background_color;
    float2 virtual_resolution;      // TERMINAL pixels, not device pixels
    float2 jitter_displacement;
    float2 scale_noise_size;
    float  time;
    float  opacity;
    float  rasterization_intensity;
    float  burn_in_last_update;
    float  burn_in_time;
    float  static_noise;
    float  screen_curvature;
    float  glowing_line;
    float  chroma_color;
    float  jitter;
    float  horizontal_sync;
    float  horizontal_sync_strength;
    float  flickering;
    float  frame_size;
    float  bloom;
};

struct DynamicVaryings {
    float4 position [[position]];
    float2 uv;
    float  brightness;
    float  distortion_scale;
    float  distortion_freq;
};

//  The per-frame RNG, and it runs in the VERTEX stage -- four invocations a
//  frame rather than one per pixel, since every pixel needs the same answer.
//
//  The constants: 2.048 is 512/250 and 1048.576 is 512*2048/1000, which walk the
//  512x512 noise texture one texel per 4ms horizontally and one row per ~2s
//  vertically.  A deterministic, texture-driven stream rather than a hash.
vertex DynamicVaryings dynamic_vertex(uint vid [[vertex_id]],
                                      constant DynamicUniforms &u [[buffer(0)]],
                                      texture2d<float> noise [[texture(0)]],
                                      sampler smp [[sampler(0)]])
{
    float2 uv = float2(float(vid & 1u), float(vid >> 1u));

    float2 coords = float2(fract(u.time / 2.048), fract(u.time / 1048.576));
    float4 n = noise.sample(smp, coords);

    DynamicVaryings out;
    out.position = float4(uv * 2.0 - 1.0, 0.0, 1.0);
    out.uv = uv;
    out.brightness = 1.0 + (n.g - 0.5) * u.flickering;

    float randval = u.horizontal_sync_strength - n.r;
    // step() first: only frames whose noise sample falls below the threshold
    // tear at all, which is what makes the tearing intermittent rather than
    // constant.  The threshold is ALSO the amplitude, so raising the slider
    // makes tearing both more frequent and more violent.
    out.distortion_scale = step(0.0, randval) * randval
                           * u.horizontal_sync_strength * u.horizontal_sync;
    out.distortion_freq = mix(4.0, 40.0, n.g) * step(0.0, u.horizontal_sync);
    return out;
}

inline float3 apply_rasterization(float2 screen_coords, float3 texel,
                                  float2 virtual_res, float intensity)
{
    if (CRT_RASTER_MODE == 0 || CRT_RASTER_MODE == 4 || intensity <= 0.0)
        return texel;

    const float INTENSITY = 0.30;
    const float BRIGHTBOOST = 0.30;

    float3 result = texel;
    if (CRT_RASTER_MODE == 3) {
        // Sub-pixels: an aperture grille, three phosphor stripes per pixel.
        const float SUBPIXELS = 3.0;
        float3 offsets = float3(3.141592654) * float3(0.5, 0.5 - 2.0 / 3.0,
                                                      0.5 - 4.0 / 3.0);
        float2 omega = float2(3.141592654) * float2(2.0) * virtual_res;
        float2 angle = screen_coords * omega;
        float3 xfactors = (SUBPIXELS + sin(angle.x + offsets)) / (SUBPIXELS + 1.0);
        result = texel * xfactors;
    }

    float3 high = ((1.0 + BRIGHTBOOST) - (0.2 * result)) * result;
    float3 low  = ((1.0 - INTENSITY) + (0.1 * result)) * result;

    float2 coords = fract(screen_coords * virtual_res) * 2.0 - float2(1.0);
    float mask;
    if (CRT_RASTER_MODE == 2) {
        // Pixels: dark in both axes, so each cell is a rounded dot.
        float2 squared = coords * coords;
        mask = 1.0 - squared.x - squared.y;
    } else {
        // Scanlines (1) and sub-pixels (3): dark between rows only.
        mask = 1.0 - abs(coords.y);
    }

    return mix(texel, mix(low, high, mask), intensity);
}

inline float3 convert_with_chroma(float3 in_color, float3 font_color,
                                  float3 background_color, float chroma)
{
    float grey = rgb2grey(in_color);
    if (CRT_CHROMA) {
        // The terminal's own colours, pulled toward the phosphor by `chroma'.
        // Dividing by grey recovers the hue before re-tinting, which is why a
        // red cell stays recognisably red at chroma 1 and becomes pure phosphor
        // at chroma 0.
        float denom = max(grey, 0.0001);
        float3 foreground = mix(font_color, in_color * font_color / denom, chroma);
        return mix(background_color, foreground, grey);
    }
    return mix(background_color, font_color, grey);
}

//  The scrolling bright band.  smoothstep over 120 rows, swept top to bottom
//  every 1/0.15 ~ 6.7 seconds.
inline float glow_line(float2 coords, float2 virtual_res, float time)
{
    return fract(smoothstep(-120.0, 0.0,
                            coords.y - (virtual_res.y + 120.0) * fract(time * 0.15)));
}

fragment float4 dynamic_fragment(DynamicVaryings in [[stage_in]],
                                 constant DynamicUniforms &u [[buffer(0)]],
                                 texture2d<float> noise [[texture(0)]],
                                 texture2d<float> screen [[texture(1)]],
                                 texture2d<float> burn_in [[texture(2)]],
                                 texture2d<float> frame [[texture(3)]],
                                 sampler noise_sampler [[sampler(0)]],
                                 sampler smp [[sampler(1)]])
{
    float2 uv = float2(in.uv.x, 1.0 - in.uv.y);
    float distance = length(float2(0.5) - uv);

    float2 static_coords = distort_coordinates(uv, u.frame_size, u.screen_curvature);
    float2 coords = uv;

    // The horizontal-sync tear: a sine in y, scrolling with time.
    coords.x += sin((coords.y + u.time) * in.distortion_freq) * in.distortion_scale;

    float4 noise_texel = noise.sample(
        noise_sampler,
        u.scale_noise_size * coords + float2(fract(u.time / 0.051),
                                             fract(u.time / 0.237)));

    // .b and .a drive the jitter, and `jitter' multiplies in a SECOND time --
    // it is already in jitter_displacement -- so the slider is effectively
    // squared.  Upstream does this; it is preserved.
    float2 txt_coords = coords + (noise_texel.ba - float2(0.5))
                                 * u.jitter_displacement * u.jitter;

    float additive = 0.0001;
    // Snow, vignetted: less at the edges, which is where a real tube's beam is
    // weakest.
    additive += noise_texel.a * u.static_noise * (1.0 - distance * 1.3);
    additive += glow_line(coords * u.virtual_resolution, u.virtual_resolution,
                          u.time) * u.glowing_line;

    float4 frame_colour = float4(0.0);
    if (CRT_DISPLAY_FRAME) {
        frame_colour = frame.sample(smp, float2(in.uv.x, 1.0 - in.uv.y));
        // No snow or glow under the bezel -- it is plastic, not phosphor.
        additive *= (1.0 - frame_colour.a);
    }

    float3 txt = screen.sample(smp, txt_coords).rgb;
    // The other half of the headroom pair; see static_fragment.
    txt *= (1.0 + max(u.bloom, 0.0));

    if (CRT_BURN_IN) {
        float4 blur = burn_in.sample(smp, static_coords);
        float decay = clamp((u.time - u.burn_in_last_update) * u.burn_in_time,
                            0.0, 1.0);
        // (1 - blur.a) is the lit mask: where the pixel is currently lit, its
        // own trail contributes nothing, so glyphs do not smear into themselves.
        float3 burn = 0.65 * (blur.rgb - float3(decay)) * (1.0 - blur.a);
        txt = max(txt, burn);
    }

    txt += float3(additive);
    // staticCoords, NOT txt_coords: the grille follows the curvature but not the
    // jitter, which is what makes jitter read as the beam moving behind it.
    txt = apply_rasterization(static_coords, txt, u.virtual_resolution,
                              u.rasterization_intensity);

    float3 final = convert_with_chroma(txt, u.font_color.rgb,
                                       u.background_color.rgb, u.chroma_color);
    final *= mix(1.0, in.brightness, step(0.0, u.flickering));

    if (CRT_DISPLAY_FRAME) {
        final = mix(final, frame_colour.rgb, frame_colour.a);
    }

    // A REQUIRED DEVIATION.  Upstream emits vec4(finalColor, qt_Opacity) --
    // straight alpha -- because Qt composites that way.  CAMetalLayer
    // composites PREMULTIPLIED, so the colour is multiplied here.  It only
    // shows when windowOpacity is below 1, and then it shows badly.
    return float4(final * u.opacity, u.opacity);
}

// ---------------------------------------------------------------------------
// A test fixture, and labelled as one.
//
//  tests/metal-tests.lisp uses this to exercise the pipeline machinery itself --
//  a specialised pipeline, a uniform block through setFragmentBytes:, a draw,
//  a readback -- separately from any effect.  When an effect test fails it is
//  useful to know whether the plumbing is sound, and that is what this answers.
// ---------------------------------------------------------------------------

struct GradientUniforms {
    float time;
    float aspect;
};

fragment float4 gradient_fragment(Varyings in [[stage_in]],
                                  constant GradientUniforms &u [[buffer(0)]])
{
    // Reusing CRT_CHROMA rather than inventing a ninth constant: the test only
    // needs SOME constant to prove specialisation happens.
    if (!CRT_CHROMA) return float4(0.0, 0.0, 1.0, 1.0);
    return float4(in.uv, 0.5 + 0.5 * sin(u.time), 1.0);
}
