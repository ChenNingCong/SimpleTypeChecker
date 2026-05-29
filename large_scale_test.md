Now let's do a large scale testing for our package
1. Firstly let's use github's open source julia repo as a target to stress test our implementation
In a few Julia POPL/PLDI papers, they mention that they run their type inferencer on many open source codes (thousands). The first step is to understand how they finish their tasks maybe we can borrow from what they have done there
2. Then, let's create the topology of all the packages:
Newer version of Julia supports package extension which can be problematic for testing purpose (which invalidate methods). But I am not sure this part please check
Then we build a DAG or graph of these packages - some packages are C wrapper or python wrapper. We ignore packages that are python wrappers or (transitively) depend on one it. For C wrapper, assign a weight to C wrapper accoding to the size.
Then let's order the package according to the priority: prefer the pure packages with few dependencies and test cases. Produce a linear order of all the packages (and discard the error ones). 
Create a UI in flask to explain each step, the first step is to download the data (if exists then skip). Do your work in scripts folder. You can think about whether it's better to use python or julia