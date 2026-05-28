# Output is formatted as expected

    Code
      cpr_rand_test(phylocom$comm, phylocom$phy, null_model = "swap", n_reps = 10,
      quiet = TRUE)
    Output
                 pd_obs pd_rand_mean pd_rand_sd   pd_obs_z pd_obs_c_upper
      clump1  0.3018868    0.4584906 0.03563325 -4.3948781              0
      clump2a 0.3207547    0.4641509 0.04464966 -3.2115862              0
      clump2b 0.3396226    0.4830189 0.02976644 -4.8173793              0
      clump4  0.4150943    0.4509434 0.04398021 -0.8151179              0
      even    0.5660377    0.4679245 0.02142061  4.5803177             10
      random  0.5094340    0.4716981 0.03444796  1.0954451              8
              pd_obs_c_lower pd_obs_q pd_obs_p_upper pd_obs_p_lower pd_alt_obs
      clump1              10       10            0.0            1.0  0.3333333
      clump2a             10       10            0.0            1.0  0.3541667
      clump2b             10       10            0.0            1.0  0.3750000
      clump4               7       10            0.0            0.7  0.4375000
      even                 0       10            1.0            0.0  0.5625000
      random               2       10            0.8            0.2  0.5208333
              pd_alt_rand_mean pd_alt_rand_sd pd_alt_obs_z pd_alt_obs_c_upper
      clump1         0.4833333     0.03074437   -4.8789427                  0
      clump2a        0.4833333     0.03227486   -4.0020828                  0
      clump2b        0.5104167     0.02820847   -4.8005681                  0
      clump4         0.4729167     0.03113404   -1.1375543                  0
      even           0.5000000     0.02196026    2.8460499                 10
      random         0.4979167     0.02680592    0.8549108                  8
              pd_alt_obs_c_lower pd_alt_obs_q pd_alt_obs_p_upper pd_alt_obs_p_lower
      clump1                  10           10                0.0                1.0
      clump2a                 10           10                0.0                1.0
      clump2b                 10           10                0.0                1.0
      clump4                   8           10                0.0                0.8
      even                     0           10                1.0                0.0
      random                   1           10                0.8                0.1
                rpd_obs rpd_rand_mean rpd_rand_sd   rpd_obs_z rpd_obs_c_upper
      clump1  0.9056604     0.9480594  0.02889592 -1.46730000               0
      clump2a 0.9056604     0.9590221  0.03945354 -1.35252111               0
      clump2b 0.9056604     0.9462078  0.02127311 -1.90604245               0
      clump4  0.9487871     0.9523440  0.04174814 -0.08519907               5
      even    1.0062893     0.9359165  0.01603741  4.38804158              10
      random  0.9781132     0.9469540  0.03698009  0.84259495               7
              rpd_obs_c_lower rpd_obs_q rpd_obs_p_upper rpd_obs_p_lower    pe_obs
      clump1               10        10             0.0             1.0 0.1333333
      clump2a               8        10             0.0             0.8 0.1081761
      clump2b              10        10             0.0             1.0 0.1286164
      clump4                3        10             0.5             0.3 0.1411950
      even                  0        10             1.0             0.0 0.2506289
      random                3        10             0.7             0.3 0.2380503
              pe_rand_mean pe_rand_sd   pe_obs_z pe_obs_c_upper pe_obs_c_lower
      clump1     0.1677358 0.02224209 -1.5467304              1              9
      clump2a    0.1696541 0.02480060 -2.4788908              0             10
      clump2b    0.1716981 0.02340134 -1.8409958              0             10
      clump4     0.1632704 0.03027039 -0.7292761              1              9
      even       0.1693711 0.03068082  2.6484902             10              0
      random     0.1582704 0.02963402  2.6921723             10              0
              pe_obs_q pe_obs_p_upper pe_obs_p_lower pe_alt_obs pe_alt_rand_mean
      clump1        10            0.1            0.9  0.1472222        0.1671528
      clump2a       10            0.0            1.0  0.1194444        0.1651042
      clump2b       10            0.0            1.0  0.1420139        0.1743056
      clump4        10            0.1            0.9  0.1454861        0.1604861
      even          10            1.0            0.0  0.2246528        0.1741667
      random        10            1.0            0.0  0.2211806        0.1587847
              pe_alt_rand_sd pe_alt_obs_z pe_alt_obs_c_upper pe_alt_obs_c_lower
      clump1      0.01482223   -1.3446397                  1                  9
      clump2a     0.02123409   -2.1503028                  0                 10
      clump2b     0.01631082   -1.9797693                  0                 10
      clump4      0.01795720   -0.8353193                  1                  9
      even        0.02709530    1.8632792                 10                  0
      random      0.02576632    2.4216047                 10                  0
              pe_alt_obs_q pe_alt_obs_p_upper pe_alt_obs_p_lower   rpe_obs
      clump1            10                0.1                0.9 0.9056604
      clump2a           10                0.0                1.0 0.9056604
      clump2b           10                0.0                1.0 0.9056604
      clump4            10                0.1                0.9 0.9705048
      even              10                1.0                0.0 1.1156280
      random            10                1.0                0.0 1.0762714
              rpe_rand_mean rpe_rand_sd  rpe_obs_z rpe_obs_c_upper rpe_obs_c_lower
      clump1      1.0019661  0.08572047 -1.1234857               1               9
      clump2a     1.0300549  0.10965468 -1.1344201               0               8
      clump2b     0.9825458  0.05753612 -1.3362985               1               9
      clump4      1.0156585  0.12320378 -0.3664962               6               4
      even        0.9692653  0.04299290  3.4043453              10               0
      random      0.9978245  0.10380152  0.7557401               8               2
              rpe_obs_q rpe_obs_p_upper rpe_obs_p_lower
      clump1         10             0.1             0.9
      clump2a        10             0.0             0.8
      clump2b        10             0.1             0.9
      clump4         10             0.6             0.4
      even           10             1.0             0.0
      random         10             0.8             0.2

