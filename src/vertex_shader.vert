#version 130
in vec3 VertexPosition;
uniform mat4 modelView;
uniform mat4 projection;
out vec3 diffuseColor;
const vec3 light = normalize(vec3(10.0, 6.0, 3.0));
const vec3 lightColor = vec3(1.0, 1.0, 0.0);
const vec3 diffuseMaterial = vec3(0.0, 1.0, 1.0);
void main(void) {
    vec3 v = normalize(VertexPosition) * 0.5 + 0.5;
    diffuseColor = vec3(dot(v, light))* lightColor * diffuseMaterial;
    gl_Position = projection *  modelView * vec4(VertexPosition, 1.0);
}