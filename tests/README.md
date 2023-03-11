Tests and Coverage
================
11 March, 2023 22:06:49

- <a href="#coverage" id="toc-coverage">Coverage</a>
- <a href="#unit-tests" id="toc-unit-tests">Unit Tests</a>

This output is created by
[covrpage](https://github.com/yonicd/covrpage).

## Coverage

Coverage summary is created using the
[covr](https://github.com/r-lib/covr) package.

| Object                                              | Coverage (%) |
|:----------------------------------------------------|:------------:|
| AquaBEHERgui                                        |    79.74     |
| [R/run_app.R](../R/run_app.R)                       |     0.00     |
| [R/app_server.R](../R/app_server.R)                 |    36.78     |
| [R/golem_utils_server.R](../R/golem_utils_server.R) |    77.78     |
| [R/mod_Home.R](../R/mod_Home.R)                     |    78.12     |
| [R/golem_utils_ui.R](../R/golem_utils_ui.R)         |    87.94     |
| [R/app_config.R](../R/app_config.R)                 |    100.00    |
| [R/app_ui.R](../R/app_ui.R)                         |    100.00    |
| [R/mod_locPET.R](../R/mod_locPET.R)                 |    100.00    |
| [R/mod_locSWB.R](../R/mod_locSWB.R)                 |    100.00    |
| [R/mod_locWSC.R](../R/mod_locWSC.R)                 |    100.00    |
| [R/mod_spPET.R](../R/mod_spPET.R)                   |    100.00    |
| [R/mod_spSWB.R](../R/mod_spSWB.R)                   |    100.00    |
| [R/mod_spWSC.R](../R/mod_spWSC.R)                   |    100.00    |

<br>

## Unit Tests

Unit Test summary is created using the
[testthat](https://github.com/r-lib/testthat) package.

| file                                                            |   n |  time | error | failed | skipped | warning | icon |
|:----------------------------------------------------------------|----:|------:|------:|-------:|--------:|--------:|:-----|
| [test-AquaBEHERgui.R](testthat/test-AquaBEHERgui.R)             |   1 | 0.031 |     0 |      0 |       0 |       0 |      |
| [test-golem_utils_server.R](testthat/test-golem_utils_server.R) |  13 | 0.057 |     0 |      0 |       0 |       0 |      |
| [test-golem_utils_ui.R](testthat/test-golem_utils_ui.R)         |  51 | 0.152 |     0 |      0 |       0 |       0 |      |
| [test-golem-recommended.R](testthat/test-golem-recommended.R)   |  10 | 0.115 |     0 |      0 |       1 |       0 | 🔶   |
| [test-mod_Home.R](testthat/test-mod_Home.R)                     |   2 | 0.011 |     0 |      0 |       0 |       0 |      |
| [test-mod_locPET.R](testthat/test-mod_locPET.R)                 |   2 | 0.065 |     0 |      0 |       0 |       0 |      |
| [test-mod_locSWB.R](testthat/test-mod_locSWB.R)                 |   2 | 0.006 |     0 |      0 |       0 |       0 |      |
| [test-mod_locWSC.R](testthat/test-mod_locWSC.R)                 |   2 | 0.007 |     0 |      0 |       0 |       0 |      |
| [test-mod_spPET.R](testthat/test-mod_spPET.R)                   |   2 | 0.006 |     0 |      0 |       0 |       0 |      |
| [test-mod_spSWB.R](testthat/test-mod_spSWB.R)                   |   2 | 0.005 |     0 |      0 |       0 |       0 |      |
| [test-mod_spWSC.R](testthat/test-mod_spWSC.R)                   |   2 | 0.006 |     0 |      0 |       0 |       0 |      |

<details open>
<summary>
Show Detailed Test Results
</summary>

| file                                                                    | context            | test                           | status  |   n |  time | icon |
|:------------------------------------------------------------------------|:-------------------|:-------------------------------|:--------|----:|------:|:-----|
| [test-AquaBEHERgui.R](testthat/test-AquaBEHERgui.R#L2)                  | AquaBEHERgui       | multiplication works           | PASS    |   1 | 0.031 |      |
| [test-golem_utils_server.R](testthat/test-golem_utils_server.R#L2)      | golem_utils_server | not_in works                   | PASS    |   2 | 0.015 |      |
| [test-golem_utils_server.R](testthat/test-golem_utils_server.R#L7)      | golem_utils_server | not_null works                 | PASS    |   2 | 0.007 |      |
| [test-golem_utils_server.R](testthat/test-golem_utils_server.R#L12)     | golem_utils_server | not_na works                   | PASS    |   2 | 0.008 |      |
| [test-golem_utils_server.R](testthat/test-golem_utils_server.R#L17_L22) | golem_utils_server | drop_nulls works               | PASS    |   1 | 0.004 |      |
| [test-golem_utils_server.R](testthat/test-golem_utils_server.R#L26_L29) | golem_utils_server | %\|\|% works                   | PASS    |   2 | 0.008 |      |
| [test-golem_utils_server.R](testthat/test-golem_utils_server.R#L37_L40) | golem_utils_server | %\|NA\|% works                 | PASS    |   2 | 0.007 |      |
| [test-golem_utils_server.R](testthat/test-golem_utils_server.R#L48_L50) | golem_utils_server | rv and rvtl work               | PASS    |   2 | 0.008 |      |
| [test-golem_utils_ui.R](testthat/test-golem_utils_ui.R#L2)              | golem_utils_ui     | Test with_red_star works       | PASS    |   2 | 0.008 |      |
| [test-golem_utils_ui.R](testthat/test-golem_utils_ui.R#L10)             | golem_utils_ui     | Test list_to_li works          | PASS    |   3 | 0.014 |      |
| [test-golem_utils_ui.R](testthat/test-golem_utils_ui.R#L22_L28)         | golem_utils_ui     | Test list_to_p works           | PASS    |   3 | 0.010 |      |
| [test-golem_utils_ui.R](testthat/test-golem_utils_ui.R#L53)             | golem_utils_ui     | Test named_to_li works         | PASS    |   3 | 0.012 |      |
| [test-golem_utils_ui.R](testthat/test-golem_utils_ui.R#L66)             | golem_utils_ui     | Test tagRemoveAttributes works | PASS    |   4 | 0.009 |      |
| [test-golem_utils_ui.R](testthat/test-golem_utils_ui.R#L82)             | golem_utils_ui     | Test undisplay works           | PASS    |   8 | 0.022 |      |
| [test-golem_utils_ui.R](testthat/test-golem_utils_ui.R#L110)            | golem_utils_ui     | Test display works             | PASS    |   4 | 0.012 |      |
| [test-golem_utils_ui.R](testthat/test-golem_utils_ui.R#L124)            | golem_utils_ui     | Test jq_hide works             | PASS    |   2 | 0.007 |      |
| [test-golem_utils_ui.R](testthat/test-golem_utils_ui.R#L132)            | golem_utils_ui     | Test rep_br works              | PASS    |   2 | 0.006 |      |
| [test-golem_utils_ui.R](testthat/test-golem_utils_ui.R#L140)            | golem_utils_ui     | Test enurl works               | PASS    |   2 | 0.005 |      |
| [test-golem_utils_ui.R](testthat/test-golem_utils_ui.R#L148)            | golem_utils_ui     | Test columns wrappers works    | PASS    |  16 | 0.042 |      |
| [test-golem_utils_ui.R](testthat/test-golem_utils_ui.R#L172)            | golem_utils_ui     | Test make_action_button works  | PASS    |   2 | 0.005 |      |
| [test-golem-recommended.R](testthat/test-golem-recommended.R#L3)        | golem-recommended  | app ui                         | PASS    |   2 | 0.087 |      |
| [test-golem-recommended.R](testthat/test-golem-recommended.R#L13)       | golem-recommended  | app server                     | PASS    |   4 | 0.012 |      |
| [test-golem-recommended.R](testthat/test-golem-recommended.R#L24_L26)   | golem-recommended  | app_sys works                  | PASS    |   1 | 0.005 |      |
| [test-golem-recommended.R](testthat/test-golem-recommended.R#L36_L42)   | golem-recommended  | golem-config works             | PASS    |   2 | 0.009 |      |
| [test-golem-recommended.R](testthat/test-golem-recommended.R#L72)       | golem-recommended  | app launches                   | SKIPPED |   1 | 0.002 | 🔶   |
| [test-mod_Home.R](testthat/test-mod_Home.R#L31)                         | mod_Home           | module ui works                | PASS    |   2 | 0.011 |      |
| [test-mod_locPET.R](testthat/test-mod_locPET.R#L31)                     | mod_locPET         | module ui works                | PASS    |   2 | 0.065 |      |
| [test-mod_locSWB.R](testthat/test-mod_locSWB.R#L31)                     | mod_locSWB         | module ui works                | PASS    |   2 | 0.006 |      |
| [test-mod_locWSC.R](testthat/test-mod_locWSC.R#L31)                     | mod_locWSC         | module ui works                | PASS    |   2 | 0.007 |      |
| [test-mod_spPET.R](testthat/test-mod_spPET.R#L31)                       | mod_spPET          | module ui works                | PASS    |   2 | 0.006 |      |
| [test-mod_spSWB.R](testthat/test-mod_spSWB.R#L31)                       | mod_spSWB          | module ui works                | PASS    |   2 | 0.005 |      |
| [test-mod_spWSC.R](testthat/test-mod_spWSC.R#L31)                       | mod_spWSC          | module ui works                | PASS    |   2 | 0.006 |      |

| Failed | Warning | Skipped |
|:-------|:--------|:--------|
| 🛑     | ⚠️      | 🔶      |

</details>
<details>
<summary>
Session Info
</summary>

| Field    | Value                        |
|:---------|:-----------------------------|
| Version  | R version 4.2.2 (2022-10-31) |
| Platform | x86_64-pc-linux-gnu (64-bit) |
| Running  | Kali GNU/Linux Rolling       |
| Language | en_US                        |
| Timezone | Africa/Addis_Ababa           |

| Package  | Version |
|:---------|:--------|
| testthat | 3.1.6   |
| covr     | 3.6.1   |
| covrpage | 0.2     |

</details>
<!--- Final Status : skipped/warning --->
