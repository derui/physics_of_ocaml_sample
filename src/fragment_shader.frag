
#version 130
out vec4 Color;
in vec3 diffuseColor;
void main() {
  Color = vec4(diffuseColor, 1.0);
}