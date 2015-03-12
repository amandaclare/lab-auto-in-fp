# lab-auto-in-fp
Laboratory automation in a functional programming language

C. Runciman,  A. Clare and R. Harkness

Published in Journal of Laboratory Automation 2014 Dec; 19(6):569-76. doi: 10.1177/2211068214543373.

http://jla.sagepub.com/content/19/6/569.abstract

After some years of use in academic and research settings, functional languages are starting to enter the mainstream as an alternative to more conventional programming languages. This article explores one way to use Haskell, a functional programming language, in the development of control programs for laboratory automation systems. We give code for an example system, discuss some programming concepts that we need for this example, and demonstrate how the use of functional programming allows us to express and verify properties of the resulting code.

`Lab-auto-in-fp.lhs` contains a complete literate Haskell program. 

To generate the tex file and the PDF:

```
lhs2TeX Lab-auto-in-fp.lhs -o Lab-auto-in-fp.tex
pdflatex Lab-auto-in-fp.tex
pdflatex Lab-auto-in-fp.tex
```