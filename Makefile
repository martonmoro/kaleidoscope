CXX = clang++
CXXFLAGS = -g -O3
TARGET = a.out

all: $(TARGET)

$(TARGET): toy.cpp
	$(CXX) $(CXXFLAGS) $< -o $@

clean:
	rm -rf $(TARGET) $(TARGET).dSYM/