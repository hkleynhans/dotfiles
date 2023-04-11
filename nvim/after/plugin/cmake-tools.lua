require("cmake-tools").setup({
    cmake_command = "cmake",
    cmake_build_directory = "",
    cmake_build_directory_prefix = "cmake_build_",
    cmake_generate_options = { "-D", "CMAKE_EXPORT_COMPILE_COMMANDS=1" },
    cmake_show_console = "always", -- "always", "only_on_error"
    cmake_variants_message = {
        short = { show = true },
	long = { show = true, max_length = 40 },
    },
})
