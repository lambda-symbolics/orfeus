// Layout shared by every Orfeus compute stage.
//
// Binding 0 is the stage's input and binding 1 its output. A stage that rewrites
// its pixels in place declares only binding 0 and leaves binding 1 statically
// unused; the host still binds a valid buffer there so one descriptor set layout
// serves every pipeline.
//
// The push-constant members are all four-component vectors so that std140 and
// std430 agree on their offsets, whatever the driver assumes.

layout(push_constant) uniform Parameters {
    // x: pixels in the image, y: input channels per pixel.
    uvec4 counts;
    // x: camera level at which a photosite counts as saturated.
    vec4 clip;
    vec4 white_balance;
    // Rows of the camera-to-linear-sRGB matrix, one channel per component.
    vec4 matrix_rows[3];
} parameters;

// The dispatch is at most 65535 groups wide, so rows continue the pixel index.
uint stage_pixel() {
    return gl_GlobalInvocationID.x + gl_GlobalInvocationID.y * 65535u * 256u;
}
