require("other-nvim").setup({
    mappings = {
        {
            pattern = "(.*).cpp$",
            target = "%1.h",
	    context = "header",
        },
        {
            pattern = "(.*).h$",
            target = "%1.cpp",
	    context = "source",
        },
        {
            pattern = "(.*).cpp$",
            target = "%1.t.cpp",
	    context = "test",
        },
        {
            pattern = "(.*).h$",
            target = "%1.t.cpp",
	    context = "test",
        },
        {
            pattern = "(.*).t.cpp$",
            target = "%1.cpp",
	    context = "source",
        },
        {
            pattern = "(.*).t.cpp$",
            target = "%1.h",
	    context = "header",
        }
    }
})
