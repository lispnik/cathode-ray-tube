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
// A gradient, for M1.
//
//  This is the shader the window draws until the effect chain lands in M3.  It
//  is not a placeholder for its own sake: it exercises the three things every
//  later pass depends on -- the synthesised quad, a uniform block delivered by
//  setFragmentBytes:, and a function constant that genuinely specialises.
// ---------------------------------------------------------------------------

struct GradientUniforms {
    float time;
    float aspect;
};

fragment float4 gradient_fragment(Varyings in [[stage_in]],
                                  constant GradientUniforms &u [[buffer(0)]])
{
    if (!CRT_CHROMA) {
        // With the constant unset the pass is a flat colour.  A test asserts
        // this, which is what proves the specialisation is real rather than a
        // branch taken at run time.
        return float4(0.0, 0.0, 1.0, 1.0);
    }
    float3 colour = float3(in.uv, 0.5 + 0.5 * sin(u.time));
    return float4(colour, 1.0);
}
