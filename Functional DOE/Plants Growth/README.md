## The Growth Simulator App
<p align="center">
  <img src="App.gif">
</p>

## The Cress Experiment
In this experiment, we investigate the impact of three factors on garden cress growth.
For additional information, you can refer to the link https://community.jmp.com/t5/JMP-Blog/Let-It-Grow-The-Garden-Cress-Challenge-Announcing-our-Winner/ba-p/549808

<div align="center">

|       |       |          |       |
| -     | -     | -        | -     |
|Factor           |       | Levels   |       |
|                 |   Low  ||     High       |
|Substrate        |Soil    ||Cotton          |
|Light Condition  |Light   ||Dark            |
|Cress Type       |Plain   ||Curled          |

<div

<p align="center">
  <img src="The Garden Cress Challenge.gif">
</p>

## Height curve as function of time for each pot
<p align="center">
  <img src="Growth Data.png">
</p>

## Smoothing the Curves
<div style="text-align: left;">
  <p>The first step of fda analysis is to smooth the discrete curves. 
  </p>
  <p align="center">
  <img src="Rplot02.png">
</p>

## Curve Decomposition and Reconstruction
<div style="text-align: left;">
  <p>Once the curve is smoothed it can be decomposed to its principle components. Like PCA, FPCA displays Functional Principal Components Analysis the dominant or modes of variation.
  </p>
  <p align="center">
  <img src="curve-reconstruction.gif">
</p>

## Curve Clustering
<div style="text-align: left;">
  <p>
  </p>
  <p align="center">
  <img src="clustering_fpc_scores.png">
</p>