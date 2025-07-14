CXX = clang++
CXXFLAGS = -g -O3 -std=c++17
LLVM_CONFIG = /opt/homebrew/opt/llvm/bin/llvm-config
LLVM_CXXFLAGS = $(shell $(LLVM_CONFIG) --cxxflags)
LLVM_LDFLAGS = $(shell $(LLVM_CONFIG) --ldflags)
LLVM_LIBS = $(shell $(LLVM_CONFIG) --libs core)
LLVM_SYSTEM_LIBS = $(shell $(LLVM_CONFIG) --system-libs)

TARGET = toy

all: $(TARGET)

$(TARGET): toy.cpp
	$(CXX) $(CXXFLAGS) $(LLVM_CXXFLAGS) $< $(LLVM_LDFLAGS) $(LLVM_LIBS) $(LLVM_SYSTEM_LIBS) -o $@

clean:
	rm -rf $(TARGET) $(TARGET).dSYM/

.PHONY: all clean