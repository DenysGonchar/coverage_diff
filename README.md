# coverage_diff

An escript to analyse coverage difference reported at [coveralls.io](https://coveralls.io/github/esl) service.<br>
Draft version of the tool created by [fenek](https://gist.github.com/fenek/43f3b6015741211dfb5bb0ad03be4a2b).

## Build

    $ rebar3 escriptize

## Run

    $ cd _build/default/bin/
    $ ./coverage_diff [-c cache_directory] [--md] build1 build2

## Arguments

    -c cache_directory
       Loading of the data from coveralls.io takes time, it has sence to cache it if 
       want to make a couple of comparisons between different builds
    --md 
       generate markdown report, useful if you want to add report as comment to GitHub.
       
## Meaning of the table columns
    Diff table:
       Cov1/Cov2 - coverage in build1/build2
       dCov      - coverage difference
       dR        - difference in number of relevant (coverable) code lines
       dN        - difference in number of noncovered (relevant) code lines
       
    New/Removed files table:
       Rel      - number of relevant code lines in the file
       Non      - number of noncovered code line
## Examples
  ### Plain text output
  
    $ ./coverage_diff -c . 14118700 14117699
    
    coverage_diff:main(["-c",".","14118700","14117699"])

    ================================================================================

    Total coverage for 14118700: 66.616%
    Total coverage for 14117699: 70.859%
    Noncovered lines count delta: 1452

    ================================================================================

    | File                           |   Cov1  |    Cov2  |     dCov  |  dR |  dN |
    |--------------------------------|---------|----------|-----------|-----|-----|
    | mongoose_riak_sup.erl          |  0.000% | 100.000% | -100.000% |   0 |  19 |
    | mam_message_xml.erl            |  0.000% | 100.000% | -100.000% |   0 |   3 |
    | mod_offline_riak.erl           |  0.000% |  96.429% |  -96.429% |   0 |  54 |
    | mod_last_riak.erl              |  0.000% |  95.238% |  -95.238% |   0 |  20 |
    | mod_roster_riak.erl            |  0.000% |  94.118% |  -94.118% |   0 |  64 |
    | mod_vcard_riak.erl             |  0.000% |  91.525% |  -91.525% |   0 |  54 |
    | mod_mam_riak_timed_arch_yz.erl |  0.000% |  87.963% |  -87.963% |   0 | 190 |
    | mongoose_riak.erl              | 13.889% |  97.222% |  -83.333% |   0 |  30 |
    | mod_private_riak.erl           |  0.000% |  81.818% |  -81.818% |   0 |  18 |
    | mod_privacy_riak.erl           |  0.000% |  81.818% |  -81.818% |   0 |  36 |
    | ejabberd_auth_riak.erl         |  0.000% |  81.818% |  -81.818% |   0 |  63 |
    | mongoose_fips.erl              | 41.176% |  75.000% |  -33.824% |  13 |   9 |
    | ejabberd_binary.erl            | 55.556% |  88.889% |  -33.333% |   0 |   3 |
    | mongoose_lib.erl               | 41.667% |  50.000% |   -8.333% |   0 |   1 |
    | ELDAPv3.erl                    | 14.709% |  22.713% |   -8.004% | 690 | 690 |
    | mod_mam_utils.erl              | 82.979% |  88.339% |   -5.360% |  -1 |  15 |
    | eldap_filter_yecc.erl          | 15.236% |  20.233% |   -4.996% | 141 | 141 |
    | mod_mam_odbc_prefs.erl         | 95.604% |  98.901% |   -3.297% |   0 |   3 |
    | mod_bosh_socket.erl            | 80.464% |  81.457% |   -0.993% |   0 |   3 |
    | p1_fsm_old.erl                 | 35.849% |  36.604% |   -0.755% |   0 |   2 |
    | mongoose_rdbms.erl             | 68.493% |  69.178% |   -0.685% |   0 |   1 |
    | ejabberd_config.erl            | 63.347% |  63.771% |   -0.424% |   0 |   2 |
    | ejabberd_app.erl               | 83.333% |  83.750% |   -0.417% |  -2 |   0 |
    | mod_mam.erl                    | 89.577% |  89.902% |   -0.326% |   0 |   1 |
    | mod_muc.erl                    | 68.144% |  68.421% |   -0.277% |   0 |   1 |

    ================================================================================

    | Removed files             | Coverage | Rel | Non |
    |---------------------------|----------|-----|-----|
    | mongoose_deprecations.erl |  96.429% |  28 |   1 |

    ================================================================================
  
  ### Markdown output

    $ ./coverage_diff --md -c . 14118700 14117699
    
    coverage_diff:main(["--md","-c",".","14118700","14117699"])

    ================================================================================

    Total coverage for 14118700: 66.616%
    Total coverage for 14117699: 70.859%
    Noncovered lines count delta: 1452

    ================================================================================

coverage decrease analysis between [14118700](https://coveralls.io/builds/14118700) and [14117699](https://coveralls.io/builds/14117699)

| File | Cov1 | Cov2 | dCov | dR | dN |
|------|------|------|------|----|----|
| mongoose_riak_sup.erl | [0.000%](https://coveralls.io/builds/14118700/source?filename=apps%2Fejabberd%2Fsrc%2Fmongoose_riak_sup.erl) | [100.000%](https://coveralls.io/builds/14117699/source?filename=apps%2Fejabberd%2Fsrc%2Fmongoose_riak_sup.erl) | -100.000% | 0 | 19 |
| mam_message_xml.erl | [0.000%](https://coveralls.io/builds/14118700/source?filename=apps%2Fejabberd%2Fsrc%2Fmam_message_xml.erl) | [100.000%](https://coveralls.io/builds/14117699/source?filename=apps%2Fejabberd%2Fsrc%2Fmam_message_xml.erl) | -100.000% | 0 | 3 |
| mod_offline_riak.erl | [0.000%](https://coveralls.io/builds/14118700/source?filename=apps%2Fejabberd%2Fsrc%2Fmod_offline_riak.erl) | [96.429%](https://coveralls.io/builds/14117699/source?filename=apps%2Fejabberd%2Fsrc%2Fmod_offline_riak.erl) | -96.429% | 0 | 54 |
| mod_last_riak.erl | [0.000%](https://coveralls.io/builds/14118700/source?filename=apps%2Fejabberd%2Fsrc%2Fmod_last_riak.erl) | [95.238%](https://coveralls.io/builds/14117699/source?filename=apps%2Fejabberd%2Fsrc%2Fmod_last_riak.erl) | -95.238% | 0 | 20 |
| mod_roster_riak.erl | [0.000%](https://coveralls.io/builds/14118700/source?filename=apps%2Fejabberd%2Fsrc%2Fmod_roster_riak.erl) | [94.118%](https://coveralls.io/builds/14117699/source?filename=apps%2Fejabberd%2Fsrc%2Fmod_roster_riak.erl) | -94.118% | 0 | 64 |
| mod_vcard_riak.erl | [0.000%](https://coveralls.io/builds/14118700/source?filename=apps%2Fejabberd%2Fsrc%2Fmod_vcard_riak.erl) | [91.525%](https://coveralls.io/builds/14117699/source?filename=apps%2Fejabberd%2Fsrc%2Fmod_vcard_riak.erl) | -91.525% | 0 | 54 |
| mod_mam_riak_timed_arch_yz.erl | [0.000%](https://coveralls.io/builds/14118700/source?filename=apps%2Fejabberd%2Fsrc%2Fmod_mam_riak_timed_arch_yz.erl) | [87.963%](https://coveralls.io/builds/14117699/source?filename=apps%2Fejabberd%2Fsrc%2Fmod_mam_riak_timed_arch_yz.erl) | -87.963% | 0 | 190 |
| mongoose_riak.erl | [13.889%](https://coveralls.io/builds/14118700/source?filename=apps%2Fejabberd%2Fsrc%2Fmongoose_riak.erl) | [97.222%](https://coveralls.io/builds/14117699/source?filename=apps%2Fejabberd%2Fsrc%2Fmongoose_riak.erl) | -83.333% | 0 | 30 |
| mod_private_riak.erl | [0.000%](https://coveralls.io/builds/14118700/source?filename=apps%2Fejabberd%2Fsrc%2Fmod_private_riak.erl) | [81.818%](https://coveralls.io/builds/14117699/source?filename=apps%2Fejabberd%2Fsrc%2Fmod_private_riak.erl) | -81.818% | 0 | 18 |
| mod_privacy_riak.erl | [0.000%](https://coveralls.io/builds/14118700/source?filename=apps%2Fejabberd%2Fsrc%2Fmod_privacy_riak.erl) | [81.818%](https://coveralls.io/builds/14117699/source?filename=apps%2Fejabberd%2Fsrc%2Fmod_privacy_riak.erl) | -81.818% | 0 | 36 |
| ejabberd_auth_riak.erl | [0.000%](https://coveralls.io/builds/14118700/source?filename=apps%2Fejabberd%2Fsrc%2Fejabberd_auth_riak.erl) | [81.818%](https://coveralls.io/builds/14117699/source?filename=apps%2Fejabberd%2Fsrc%2Fejabberd_auth_riak.erl) | -81.818% | 0 | 63 |
| mongoose_fips.erl | [41.176%](https://coveralls.io/builds/14118700/source?filename=apps%2Fejabberd%2Fsrc%2Fmongoose_fips.erl) | [75.000%](https://coveralls.io/builds/14117699/source?filename=apps%2Fejabberd%2Fsrc%2Fmongoose_fips.erl) | -33.824% | 13 | 9 |
| ejabberd_binary.erl | [55.556%](https://coveralls.io/builds/14118700/source?filename=apps%2Fejabberd%2Fsrc%2Fejabberd_binary.erl) | [88.889%](https://coveralls.io/builds/14117699/source?filename=apps%2Fejabberd%2Fsrc%2Fejabberd_binary.erl) | -33.333% | 0 | 3 |
| mongoose_lib.erl | [41.667%](https://coveralls.io/builds/14118700/source?filename=apps%2Fejabberd%2Fsrc%2Fmongoose_lib.erl) | [50.000%](https://coveralls.io/builds/14117699/source?filename=apps%2Fejabberd%2Fsrc%2Fmongoose_lib.erl) | -8.333% | 0 | 1 |
| ELDAPv3.erl | [14.709%](https://coveralls.io/builds/14118700/source?filename=apps%2Fejabberd%2Fsrc%2FELDAPv3.erl) | [22.713%](https://coveralls.io/builds/14117699/source?filename=apps%2Fejabberd%2Fsrc%2FELDAPv3.erl) | -8.004% | 690 | 690 |
| mod_mam_utils.erl | [82.979%](https://coveralls.io/builds/14118700/source?filename=apps%2Fejabberd%2Fsrc%2Fmod_mam_utils.erl) | [88.339%](https://coveralls.io/builds/14117699/source?filename=apps%2Fejabberd%2Fsrc%2Fmod_mam_utils.erl) | -5.360% | -1 | 15 |
| eldap_filter_yecc.erl | [15.236%](https://coveralls.io/builds/14118700/source?filename=apps%2Fejabberd%2Fsrc%2Feldap_filter_yecc.erl) | [20.233%](https://coveralls.io/builds/14117699/source?filename=apps%2Fejabberd%2Fsrc%2Feldap_filter_yecc.erl) | -4.996% | 141 | 141 |
| mod_mam_odbc_prefs.erl | [95.604%](https://coveralls.io/builds/14118700/source?filename=apps%2Fejabberd%2Fsrc%2Fmod_mam_odbc_prefs.erl) | [98.901%](https://coveralls.io/builds/14117699/source?filename=apps%2Fejabberd%2Fsrc%2Fmod_mam_odbc_prefs.erl) | -3.297% | 0 | 3 |
| mod_bosh_socket.erl | [80.464%](https://coveralls.io/builds/14118700/source?filename=apps%2Fejabberd%2Fsrc%2Fmod_bosh_socket.erl) | [81.457%](https://coveralls.io/builds/14117699/source?filename=apps%2Fejabberd%2Fsrc%2Fmod_bosh_socket.erl) | -0.993% | 0 | 3 |
| p1_fsm_old.erl | [35.849%](https://coveralls.io/builds/14118700/source?filename=apps%2Fejabberd%2Fsrc%2Fp1_fsm_old.erl) | [36.604%](https://coveralls.io/builds/14117699/source?filename=apps%2Fejabberd%2Fsrc%2Fp1_fsm_old.erl) | -0.755% | 0 | 2 |
| mongoose_rdbms.erl | [68.493%](https://coveralls.io/builds/14118700/source?filename=apps%2Fejabberd%2Fsrc%2Frdbms%2Fmongoose_rdbms.erl) | [69.178%](https://coveralls.io/builds/14117699/source?filename=apps%2Fejabberd%2Fsrc%2Frdbms%2Fmongoose_rdbms.erl) | -0.685% | 0 | 1 |
| ejabberd_config.erl | [63.347%](https://coveralls.io/builds/14118700/source?filename=apps%2Fejabberd%2Fsrc%2Fejabberd_config.erl) | [63.771%](https://coveralls.io/builds/14117699/source?filename=apps%2Fejabberd%2Fsrc%2Fejabberd_config.erl) | -0.424% | 0 | 2 |
| ejabberd_app.erl | [83.333%](https://coveralls.io/builds/14118700/source?filename=apps%2Fejabberd%2Fsrc%2Fejabberd_app.erl) | [83.750%](https://coveralls.io/builds/14117699/source?filename=apps%2Fejabberd%2Fsrc%2Fejabberd_app.erl) | -0.417% | -2 | 0 |
| mod_mam.erl | [89.577%](https://coveralls.io/builds/14118700/source?filename=apps%2Fejabberd%2Fsrc%2Fmod_mam.erl) | [89.902%](https://coveralls.io/builds/14117699/source?filename=apps%2Fejabberd%2Fsrc%2Fmod_mam.erl) | -0.326% | 0 | 1 |
| mod_muc.erl | [68.144%](https://coveralls.io/builds/14118700/source?filename=apps%2Fejabberd%2Fsrc%2Fmod_muc.erl) | [68.421%](https://coveralls.io/builds/14117699/source?filename=apps%2Fejabberd%2Fsrc%2Fmod_muc.erl) | -0.277% | 0 | 1 |

    ================================================================================

| Removed files | Coverage | Rel | Non |
|---------------|----------|-----|-----|
| [mongoose_deprecations.erl](https://coveralls.io/builds/14117699/source?filename=apps%2Fejabberd%2Fsrc%2Fmongoose_deprecations.erl) | 96.429% | 28 | 1 |

    ================================================================================

