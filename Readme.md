# My compiler

## Setup
llvm
```
git clone --depth 1 https://github.com/llvm/llvm-project.git
cd llvm-project
git fetch origin release/17.x
git branch release/17.x FETCH_HEAD
git checkout release/17.x
mkdir build && cd build
cmake ../llvm -DCMAKE_INSTALL_PREFIX=$HOME/llvm-17 -DCMAKE_BUILD_TYPE=Debug
cmake --build . --target install
```