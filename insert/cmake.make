build_dir := build
compile_commands_file := $(build_dir)/compile_commands.json

.PHONY: cmake build
cmake:
	mkdir -p $(build_dir)
	env -C $(build_dir) cmake ..
	ls $(compile_commands_file) >> /dev/null 2>&1 \
	&& mv $(compile_commands_file) ./

build:
	cmake --build $(build_dir)
