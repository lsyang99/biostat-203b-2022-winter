*Lin-Syuan Yang*

### Overall Grade: 106/120

### Quality of report: 10/10

-   Is the homework submitted (git tag time) before deadline? Take 10 pts per day for late submission.  

    Yes. January 21, 5:16 PM PST.

-   Is the final report in a human readable format html? 

    Yes.

-   Is the report prepared as a dynamic document (R markdown) for better reproducibility?

    Yes.

-   Is the report clear (whole sentences, typos, grammar)? Do readers have a clear idea what's going on and how results are produced by just reading the report? Take some points off if the solutions are too succint to grasp, or there are too many typos/grammar. 

    Yes.

### Completeness, correctness and efficiency of solution: 60/70

- Q1 (10/10)

- Q2 (20/20)

- Q3 (10/20)

    3.4 - lines 91:96, you are reading in zip files, so please use zcat. Otherwise you are listing the number of compressed rows. `-5`.
    
    3.5 - same issue with zcat. Then for the second part of the question, you must use sort | uniq much like you did in 3.6. `-5`.

- Q4 (10/10)

- Q5 (10/10)
	    
### Usage of Git: 10/10

-   Are branches (`main` and `develop`) correctly set up? Is the hw submission put into the `main` branch?

    Yes.

-   Are there enough commits (>=5) in develop branch? Are commit messages clear? The commits should span out not clustered the day before deadline. 

    Yes.
          
-   Is the hw1 submission tagged? 

    Yes.

-   Are the folders (`hw1`, `hw2`, ...) created correctly? 

    Yes.
  
-   Do not put a lot auxiliary files into version control. 

-   If those gz data files or `pg42671` are in Git, take 5 points off.

### Reproducibility: 10/10

-   Are the materials (files and instructions) submitted to the `main` branch sufficient for reproducing all the results? Just click the `knit` button will produce the final `html` on teaching server? 

    Yes.

-   If necessary, are there clear instructions, either in report or in a separate file, how to reproduce the results?

### R code style: 16/20

For bash commands, only enforce the 80-character rule. Take 2 pts off for each violation. 

-   [Rule 3.](https://google.github.io/styleguide/Rguide.xml#linelength) The maximum line length is 80 characters. 

    Questions 3.5 and 3.6, lines 109:131 go over 80 characters. `-4`.

-   [Rule 4.](https://google.github.io/styleguide/Rguide.xml#indentation) When indenting your code, use two spaces.

-   [Rule 5.](https://google.github.io/styleguide/Rguide.xml#spacing) Place spaces around all binary operators (=, +, -, &lt;-, etc.). 
	
-   [Rule 5.](https://google.github.io/styleguide/Rguide.xml#spacing) Do not place a space before a comma, but always place one after a comma. 

-   [Rule 5.](https://google.github.io/styleguide/Rguide.xml#spacing) Place a space before left parenthesis, except in a function call.

-   [Rule 5.](https://google.github.io/styleguide/Rguide.xml#spacing) Do not place spaces around code in parentheses or square brackets.
