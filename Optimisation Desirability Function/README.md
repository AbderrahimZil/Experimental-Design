## Multiple Responses

In the paper linked bellow, the Desirability Function is developed to optimize multiple respnses simultaneously.
The iseda behind the proposed method is that a solution to the optimization broblem is rejected if one of the optimized features is outside a desirable interval. Therefore the goal is to find operating settings that garantee compliance with the criteria of the involved responses.

The individual responses are aggregated into a composite function called the Overall Desirability which is optimized afterwards.
<p align="center">
  <img src="single_desirabilities.png">
</p>

A Central Composite Design (CCD) is used 
<p align="center">
  <img src="Plot_Desirability.gif">
</p>

\text{Overall Desirability} = (\prod_{r=1}^{R} \text{desirability}_r)^{ \frac{1}{R} }

**The Cauchy-Schwarz Inequality**
$$\left \text{Overall Desirability} = (\prod_{r=1}^{R} \text{desirability}_r)^{ \frac{1}{R} } \right$$

* Derringer and Suich (1980), "Simultaneous Optimization of Several Response Variables", 
Journal of Quality Technology, Vol. 12, No. 4, pp. 214-219.