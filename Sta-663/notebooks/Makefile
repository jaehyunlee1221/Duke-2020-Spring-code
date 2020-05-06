
driver: my_math.o my_stats.o
	g++ my_driver.cpp my_math.o my_stats.o -o my_driver
    
my_math.o: my_math.cpp my_math.hpp
	g++ -c my_math.cpp
    
my_stats.o: my_stats.cpp my_stats.hpp
	g++ -c my_stats.cpp