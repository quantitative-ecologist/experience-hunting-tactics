---
author: "Anonymous for review"
title: "Data exploration"
date: 2021-05-20
---



# 1. Prepare the data
**1.1 Load the required packages.**

```julia
using Pkg, Weave, CSV, DataFrames, Plots
```




**1.2 Import the data.**

```julia
data = DataFrame(CSV.File("data/merged-data.csv"))
```

```
70831×58 DataFrame
   Row │ mirrors_id  match_id                          timestamp           
  p ⋯
       │ String      String                            String              
  S ⋯
───────┼───────────────────────────────────────────────────────────────────
─────
     1 │ VNUXG3292T  1205AE3646F30FE2A709F7822E38A9BE  2019-03-23T21:25:33Z
  P ⋯
     2 │ VNUXG3292T  23D811A34061519DFE452BB2A1A4AADD  2019-03-24T00:06:48Z
  P
     3 │ VNUXG3292T  43C9DFA446BBF308C2099AB2355FF031  2019-06-10T10:38:14Z
  P
     4 │ VNUXG3292T  445D8ABE40BA0C39C217D0BE061AFEC5  2019-03-23T11:47:49Z
  P
     5 │ VNUXG3292T  48D0B7754A1CD9C1370BB7BC9F2C2859  2019-03-30T21:49:12Z
  P ⋯
     6 │ VNUXG3292T  4EA2D16D41599258A08A829A4EB3D97C  2019-03-23T21:44:06Z
  P
     7 │ VNUXG3292T  6FBFE77740FB421A363B7BBD7F539663  2019-03-27T20:03:17Z
  P
     8 │ VNUXG3292T  8F442A354ACAB763CB5877B5EF24D94D  2019-03-22T19:37:22Z
  P
   ⋮   │     ⋮                      ⋮                           ⋮          
    ⋱
 70825 │ MGFMQ3642M  4AB4706408D6B0702072FEEA00F9436A  2019-03-24T22:48:49Z
  P ⋯
 70826 │ MGFMQ3642M  6EAF08EE08D6B06B38538EDC3D714C10  2019-03-24T22:13:38Z
  P
 70827 │ MGFMQ3642M  8C87F61F08D6AE3662E3127EC98AB861  2019-03-22T02:50:35Z
  P
 70828 │ MGFMQ3642M  B36B278D08D6B06D3663A7C28CD71629  2019-03-24T22:28:03Z
  P
 70829 │ MGFMQ3642M  C02ABB9C08D6AE38FD255FF43140E851  2019-03-22T03:09:02Z
  P ⋯
 70830 │ MGFMQ3642M  D3A81F8A08D6B0725FA3A52F5A8257A6  2019-03-24T23:04:54Z
  P
 70831 │ MGFMQ3642M  E28A2D1208D6AD34AE85CB7465A087EA  2019-03-20T20:05:59Z
  P
                                               55 columns and 70816 rows om
itted
```





# 2. Inspect the data

**2.1 Distribution of predator behaviour**

Standard variables
```julia
# Predator behaviour
a = histogram(data.Zspeed, bins = 50, label = "Pred. speed")
b = histogram(data.Zspace_covered_rate, bins = 50, label = "Pred. space")
c = histogram(data.Zprox_mid_guard, bins = 50, label = "Pred. ambush time")
d = histogram(data.Zhook_start_time, bins = 50, label = "Pred. latency 1st capt.")

plot(a, b, c, d, layout = 4)
```

![](code/data-exploration-figs/data-exploration_4_1.png)


Square root variables
```julia
# Square root of predator behaviour
a1 = histogram(data.sqrtspeed, bins = 50, label = "Pred. speed")
b1 = histogram(data.sqrtspace_covered_rate, bins = 50, label = "Pred. space")
c1 = histogram(data.sqrtprox_mid_guard, bins = 50, label = "Pred. ambush time")
d1 = histogram(data.sqrthook_start_time, bins = 50, label = "Pred. latency 1st capt.")

plot(a1, b1, c1, d1, layout = 4)
```

![](code/data-exploration-figs/data-exploration_5_1.png)


Log variables
```julia
# Log of predator behaviour
a2 = histogram(data.logspeed, bins = 50, label = "Pred. speed")
b2 = histogram(data.logspace_covered_rate, bins = 50, label = "Pred. space")
c2 = histogram(data.logprox_mid_guard, bins = 50, label = "Pred. ambush time")
d2 = histogram(data.loghook_start_time, bins = 50, label = "Pred. latency 1st capt.")

plot(a2, b2, c2, d2, layout = 4)
```

![](code/data-exploration-figs/data-exploration_6_1.png)


**2.1 Distribution of prey behaviour**

```julia
# Prey behaviour
e = histogram(data.surv_speed, bins = 50, label = "Prey speed")
f = histogram(data.surv_space_covered_rate, bins = 50, label = "Prey space")
#g = histogram(data.boldness, bins = 50, label = "Prey boldness")
plot(e, f, layout = 2, size = (600, 200))
```

![](code/data-exploration-figs/data-exploration_7_1.png)
