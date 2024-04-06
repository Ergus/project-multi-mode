# Readme

This is a generic backend project for project.el it includes at the
moment automatic recognition for CMake, Autotools and Meson projects.

This basic code recognizes automatically the backend type and the root
directory. It also has the concept of build directory where the
`project-compile` command will execute.

The build command will be set depending of the backend automatically
and the plan is to add other generic commands like test and generate
in the future.
