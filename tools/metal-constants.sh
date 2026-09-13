#!/bin/sh
# tools/metal-constants.sh -- read Metal's enumerations out of the SDK headers
# and print them as a Lisp form.
#
# `objc' has no constant tables and will not grow any: it sends messages, and a
# message does not know what 70 means.  So the numbers have to come from
# somewhere, and the choice is between remembering them and reading them.  This
# reads them.
#
#   tools/metal-constants.sh > /tmp/constants.sexp   # regenerate
#   make check-metal-constants                       # assert nothing moved
#
# Apple has never renumbered one of these and almost certainly never will; the
# point is not that they drift but that a typo in a hand-copied table produces a
# black screen with no error, and this makes that class of bug impossible.
set -eu

SDK="$(xcrun --show-sdk-path)/System/Library/Frameworks/Metal.framework/Headers"
[ -d "$SDK" ] || { echo "no Metal headers at $SDK" >&2; exit 1; }

emit() {                       # emit <lisp-name> <C-name> [header-hint]
    # Anything between the name and the `=' is availability attributes, and
    # there can be several -- MTLStorageModeManaged carries API_AVAILABLE and
    # API_UNAVAILABLE both.  The leading anchor keeps MTLResourceStorageMode*
    # from matching MTLStorageMode*, and requiring a non-identifier character
    # after the name keeps MTLPixelFormatRGBA8Unorm from matching its _sRGB
    # sibling.
    # EVERY header, not the one named.  Apple moves these between Xcode
    # versions -- MTLDataType lived in MTLArgument.h before it had its own file
    # -- and a run that fails because a constant is in a different file than it
    # was is a false alarm about nothing.  The third argument is kept as a hint
    # for a reader, and is not used to restrict the search.
    value=$(grep -hoE "^[[:space:]]*$2([^A-Za-z0-9_][^=]*)?=[[:space:]]*(0x)?[0-9a-fA-F]+" \
              "$SDK"/*.h 2>/dev/null | head -1 \
            | sed -E 's/.*=[[:space:]]*//')
    [ -n "$value" ] || { echo "could not find $2 in any header under $SDK" >&2; exit 1; }
    case "$value" in 0x*|0X*) value=$(printf '%d' "$value") ;; esac
    printf '  (%s . %s)\n' "$1" "$value"
}

echo "("
emit pixel-format-r8unorm       MTLPixelFormatR8Unorm        MTLPixelFormat.h
emit pixel-format-rgba8unorm    MTLPixelFormatRGBA8Unorm     MTLPixelFormat.h
emit pixel-format-bgra8unorm    MTLPixelFormatBGRA8Unorm     MTLPixelFormat.h
emit pixel-format-rgba16float   MTLPixelFormatRGBA16Float    MTLPixelFormat.h
emit texture-type-2d            MTLTextureType2D             MTLTexture.h
emit usage-shader-read          MTLTextureUsageShaderRead    MTLTexture.h
emit usage-shader-write         MTLTextureUsageShaderWrite   MTLTexture.h
emit usage-render-target        MTLTextureUsageRenderTarget  MTLTexture.h
emit storage-mode-shared        MTLStorageModeShared         MTLResource.h
emit storage-mode-managed       MTLStorageModeManaged        MTLResource.h
emit storage-mode-private       MTLStorageModePrivate        MTLResource.h
emit load-action-dont-care      MTLLoadActionDontCare        MTLRenderPass.h
emit load-action-load           MTLLoadActionLoad            MTLRenderPass.h
emit load-action-clear          MTLLoadActionClear           MTLRenderPass.h
emit store-action-dont-care     MTLStoreActionDontCare       MTLRenderPass.h
emit store-action-store         MTLStoreActionStore          MTLRenderPass.h
emit primitive-triangle         MTLPrimitiveTypeTriangle     MTLRenderCommandEncoder.h
emit primitive-triangle-strip   MTLPrimitiveTypeTriangleStrip MTLRenderCommandEncoder.h
emit data-type-float            MTLDataTypeFloat             MTLDataType.h
emit data-type-int              MTLDataTypeInt               MTLDataType.h
emit data-type-bool             MTLDataTypeBool              MTLDataType.h
emit filter-nearest             MTLSamplerMinMagFilterNearest MTLSampler.h
emit filter-linear              MTLSamplerMinMagFilterLinear  MTLSampler.h
emit address-clamp-to-edge      MTLSamplerAddressModeClampToEdge MTLSampler.h
emit address-repeat             MTLSamplerAddressModeRepeat      MTLSampler.h
emit blend-factor-one           MTLBlendFactorOne                MTLRenderPipeline.h
emit blend-factor-zero          MTLBlendFactorZero               MTLRenderPipeline.h
emit blend-factor-src-alpha     MTLBlendFactorSourceAlpha        MTLRenderPipeline.h
emit blend-factor-one-minus-src-alpha MTLBlendFactorOneMinusSourceAlpha MTLRenderPipeline.h
emit blend-operation-add        MTLBlendOperationAdd             MTLRenderPipeline.h
echo ")"
