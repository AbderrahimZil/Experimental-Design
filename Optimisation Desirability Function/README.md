## Multiple Responses

In the paper linked bellow, the Desirability Function is developed to optimize multiple respnses simultaneously.
The iseda behind the proposed method is that a solution to the optimization broblem is rejected if one of the optimized features is outside a desirable interval. Therefore the goal is to find operating settings that garantee compliance with the criteria of the involved responses.

<div align="center">Some test</div>

| Response | Desired range |
| --- | --- |
| Y1 | 120 < Y1 |
| Y2 | 1000 < Y2 |
| Y3 | 400 < Y3 < 600 |
| Y4 | 60 < Y4 < 75 |

The individual responses are aggregated into a composite function called the Overall Desirability which is optimized afterwards.

**The Desirability Function**
$$\text{Overall Desirability} = (\prod_{r=1}^{R} \text{desirability}_r)^{ \frac{1}{R} } \$$
<p align="center">
  <img src="single_desirabilities.png">
</p>

A Central Composite Design (CCD) is used 
<p align="center">
  <img src="Plot_Desirability.gif">
</p>

* Derringer and Suich (1980), "Simultaneous Optimization of Several Response Variables", 
Journal of Quality Technology, Vol. 12, No. 4, pp. 214-219.