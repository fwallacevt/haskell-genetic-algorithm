# haskell-genetic-algorithm

This project was completed as part of "CSCI 0413: Functional Programming" at Middlebury College. The project was completed in collaboration with Jackson Yang, and based off of work by Kory Becker (http://www.primaryobjects.com/2013/01/27/using-artificial-intelligence-to-write-self-modifying-improving-programs/).

The goal of this project was to develop a simple genetic algorithm capable of generating short programs in Brainfuck. This was based off of previous work done by Kory Becker and, subsequently, a similar genetic algorithm developed in Java by Davin Chia and myself (https://github.com/davinchia/genetic-algorithm-java-brainfuck). Our goal in implementing the algorithm in Haskell was to force decomposition of the problem into its simplest components and separation of the "pure" and "unpure" components. Because of the constraints imposed by our particular implementation, the algorithm is not amenable to threading; altering the algorithm to allow threading is the next step in this project.

The algorithm requires the following parameters to run:
  (1) Mutation Rate: A number between 0 and 99; 95 is recommended
	(2) Crossover Rate: A number between 0 and 99; 95 is recommended
	(3) Chromosome Length: The length of the BF code; 60 is recommended 
      for short strings ("hi"), but this may need 
      to be lengthened for longer strings.
	(4) Population Size: The number of individuals in each population; 20-30 
      is recommended.
	(5) Target String: The string to be printed by the generated BF code. We 
      recommend testing with short, simple strings like "hi", 
      or even a single character. The algorithm is capable of 
      handling longer strings like "hello", and more complex 
      ones like "Hi!", but these take very long to run, and 
      require a longer chromosome length for the BF code.

The algorithm is capable of handling longer and more complex strings (strings including uppercase letters and punctuation), but may take an excessively long time to generate these more complex outputs. For testing purposes, we have mainly used the string "hi".
