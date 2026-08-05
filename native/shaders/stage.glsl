// Layout shared by every Orfeus compute stage.
//
// Binding 0 holds the image the stage works on. The push-constant members are
// four-component vectors so that std140 and std430 agree on their offsets,
// whatever the driver assumes.

layout(push_constant) uniform Parameters {
    // x: pixels in the image, y: input channels per pixel.
    uvec4 counts;
} parameters;

// The dispatch is at most 65535 groups wide, so rows continue the pixel index.
uint stage_pixel() {
    return gl_GlobalInvocationID.x + gl_GlobalInvocationID.y * 65535u * 256u;
}
