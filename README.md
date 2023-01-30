<img src="title.gif" width="500">

Statistical Rethinking (2023 Edition)
===============

Instructor: Richard McElreath

Lectures: Uploaded and pre-recorded, two per week

Discussion: Online (Zoom), Fridays 3pm-4pm Central European (Berlin) Time

# Purpose

This course teaches data analysis, but it focuses on scientific models. The unfortunate truth about data is that nothing much can be done with it, until we say what caused it. We will prioritize conceptual, causal models and precise questions about those models. We will use Bayesian data analysis to connect scientific models to evidence. And we will learn powerful computational tools for coping with high-dimension, imperfect data of the kind that biologists and social scientists face.

# Format

Online, flipped instruction. I will pre-record the lectures each week. We'll meet online once a week for an hour to discuss the material. The discussion time (3-4pm Berlin Time) should allow people in the Americas to join in their morning.

We'll use the 2nd edition of my book, <[Statistical Rethinking](https://xcelab.net/rm/statistical-rethinking/)>, and possibly some draft chapters for the 3rd edition. I'll provide a PDF of the book to enrolled students.

Registration: Closed.

# Calendar & Topical Outline

There are 10 weeks of instruction. Links to lecture recordings will appear in this table. Weekly problem sets are assigned on Fridays and due the next Friday, when we discuss the solutions in the weekly online meeting.

Full lecture playlist: <[Statistical Rethinking 2023 Playlist](https://www.youtube.com/watch?v=FdnMWdICdRs&list=PLDcUM9US4XdPz-KxHM4XHt7uUVGWWVSus)>

Note about slides: In some browsers, the slides don't show correctly. If points are missing from plots, download the slides PDF instead of viewing in browser.

| Week ## | Meeting date | Reading | Lectures |
| ------- | -------------- | ------------- | ---------------------- |
| Week 01 | 06 January  | Chapters 1, 2 and 3 | [1] <[Science Before Statistics](https://www.youtube.com/watch?v=FdnMWdICdRs&list=PLDcUM9US4XdPz-KxHM4XHt7uUVGWWVSus&index=1)> <[Slides](https://speakerdeck.com/rmcelreath/statistical-rethinking-2023-lecture-01)> <br> [2] <[Garden of Forking Data](https://www.youtube.com/watch?v=R1vcdhPBlXA&list=PLDcUM9US4XdPz-KxHM4XHt7uUVGWWVSus&index=2)> <[Slides](https://speakerdeck.com/rmcelreath/statistical-rethinking-2023-lecture-02)>
| Week 02 | 13 January | Chapter 4 | [3] <[Geocentric Models](https://www.youtube.com/watch?v=tNOu-SEacNU&list=PLDcUM9US4XdPz-KxHM4XHt7uUVGWWVSus&index=3)> <[Slides](https://speakerdeck.com/rmcelreath/statistical-rethinking-2023-lecture-03)> <br> [4] <[Categories and Curves](https://www.youtube.com/watch?v=F0N4b7K_iYQ&list=PLDcUM9US4XdPz-KxHM4XHt7uUVGWWVSus&index=4)> <[Slides](https://speakerdeck.com/rmcelreath/statistical-rethinking-2023-lecture-04)>
| Week 03 | 20 January | Chapters 5 and 6 |  [5] <[Elemental Confounds](https://www.youtube.com/watch?v=mBEA7PKDmiY&list=PLDcUM9US4XdPz-KxHM4XHt7uUVGWWVSus&index=5)> <[Slides](https://speakerdeck.com/rmcelreath/statistical-rethinking-2023-lecture-05)> <br> [6] <[Good and Bad Controls](https://www.youtube.com/watch?v=uanZZLlzKHw&list=PLDcUM9US4XdPz-KxHM4XHt7uUVGWWVSus&index=6)> <[Slides](https://speakerdeck.com/rmcelreath/statistical-rethinking-2023-lecture-06)>
| Week 04 | 27 January | Chapters 7,8,9 | [7] <[Overfitting](https://www.youtube.com/watch?v=1VgYIsANQck&list=PLDcUM9US4XdPz-KxHM4XHt7uUVGWWVSus&index=7)> <[Slides](https://speakerdeck.com/rmcelreath/statistical-rethinking-2023-lecture-07)> <br> [8] <[MCMC](https://www.youtube.com/watch?v=rZk2FqX2XnY&list=PLDcUM9US4XdPz-KxHM4XHt7uUVGWWVSus&index=8)> <[Slides](https://speakerdeck.com/rmcelreath/statistical-rethinking-2023-lecture-08)>
| Week 05 | 03 February | Chapters 10 and 11 | [9] <[Modeling Events]> <[Slides](https://speakerdeck.com/rmcelreath/statistical-rethinking-2023-lecture-09)> <br> [10] Poisson GLMs
| Week 06 | 10 February | Chapters 11 and 12 | [11] Ordered Categories <br> [12] Multilevel Models
| Week 07 | 17 February | Chapter 13 | [13] Multi-Multilevel Models <br> [14] More Multilevel Models
| Week 08 | 24 February | Chapter 14 | [15] Social networks <br> [16] Gaussian Processes
| Week 09 | 03 March | Chapter 15 | [17] Measurement Error <br> [18] Missing Data
| Week 10 | 10 March | Chapters 16 and 17 | [19] Beyond GLMs: State-space Models, ODEs <br> [20] Horoscopes


# Coding

This course involves a lot of scripting. Students can engage with the material using either the original R code examples or one of several conversions to other computing environments. The conversions are not always exact, but they are rather complete. Each option is listed below.

## Original R Flavor

For those who want to use the original R code examples in the print book, you need to install the `rethinking` R package. The code is all on github <https://github.com/rmcelreath/rethinking/> and there are additional details about the package there, including information about using the more-up-to-date `cmdstanr` instead of `rstan` as the underlying MCMC engine.

## R + Tidyverse + ggplot2 + brms

The <[Tidyverse/brms](https://bookdown.org/content/4857/)> conversion is very high quality and complete through Chapter 14.

## Python and PyMC3

The <[Python/PyMC3](https://github.com/pymc-devs/resources/tree/master/Rethinking_2)> conversion is quite complete.

## Julia and Turing

The <[Julia/Turing](https://github.com/StatisticalRethinkingJulia)> conversion is not as complete, but is growing fast and presents the Rethinking examples in multiple Julia engines, including the great <[TuringLang](https://github.com/StatisticalRethinkingJulia/TuringModels.jl)>.

## Other

The are several other conversions. See the full list at <https://xcelab.net/rm/statistical-rethinking/>.

# Homework and solutions

I will also post problem sets and solutions. Check the folders at the top of the repository.




