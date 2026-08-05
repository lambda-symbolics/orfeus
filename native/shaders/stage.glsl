// Layout shared by every Orfeus compute stage.
//
// Binding 0 is one storage buffer holding every plane a stage needs, and the
// push constants say where each plane starts. That keeps one descriptor set and
// one pipeline layout for all stages, and lets a whole multi-pass filter record
// as a single submission with no descriptor rewriting between dispatches.
//
// The push-constant members are four-component vectors so that std140 and
// std430 agree on their offsets, whatever the driver assumes.

layout(push_constant) uniform Parameters {
    // x: pixels in the image, y: width, z: height, w: tap spacing.
    uvec4 counts;
    // First element of each plane this stage reads or writes.
    uvec4 offsets;
    // x: blend amount or filter strength.
    vec4 scalars;
    // x: 0 horizontal, 1 vertical. y: stage-specific mode.
    uvec4 flags;
} parameters;

// The dispatch is at most 65535 groups wide, so rows continue the pixel index.
uint stage_pixel() {
    return gl_GlobalInvocationID.x + gl_GlobalInvocationID.y * 65535u * 256u;
}
