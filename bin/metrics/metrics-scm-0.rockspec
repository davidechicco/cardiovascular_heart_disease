package = "metrics"
version = "scm-0"

source = {
   url = "git://github.com/hpenedones/metrics",
   tag = "master"
}

description = {
   summary = "A metrics package for Torch",
   detailed = [[
   	    Computes ROC curves, confusion matrix, etc.
   ]],
   homepage = "https://github.com/hpenedones/metrics"
}

dependencies = {
   "torch >= 7.0"
}

build = {
   type = "command",
   build_command = [[
cmake -E make_directory build;
cd build;
cmake .. -DCMAKE_BUILD_TYPE=Release -DCMAKE_PREFIX_PATH="$(LUA_BINDIR)/.." -DCMAKE_INSTALL_PREFIX="$(PREFIX)"; 
$(MAKE)
   ]],
   install_command = "cd build && $(MAKE) install"
}
