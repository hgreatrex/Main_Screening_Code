
## Code & Datasets for a Large-n systematic mapping of flash flood literature

We reviewed abstracts of peer-reviewed work published between January 2000 and July 2020 in scientific journals, technical conferences and multi-author books. A total of 3748 abstracts from Web of Science Core collection were screened for relevance. 

We screened all abstracts for relevance to flash floods and then further sorted relevant abstracts into three categories: core understanding, geophysical, and social. 

We then did a secondary screening that further categorized and tagged the papers in the social category. This refined category will be turned into a searchable dataset for general use. Future work will hopefully automate this process and create tools for large reviews of social impact hazards. 

### THIS IS THE 2024 CLEAR VERSION Understanding Our File Structure

This directories in this GitHub are organized to follow the timeline of use and creation. 

within `/data` 

> `01_Web-Of-Science_Output` contines the original .bib files pulled from the Web of Science Database. This service only allows downloading in 500 item groupings. 

> `02_Data-ready-to-screen` Contains the backup output of the code 01_Screen1Code/Stage 1A Set-up code.R .  All the web of science data has been merged, pulled into sentence case and quality controlled.  The main output of this code is stored within the main /data folder as MainScreeningData_Screen1.RData.

> `03_Screen1Backup` Contains backup copies of the dataframe MainScreeningData_Screen1.RData as screening 1 is happening.  Each one is timestamped to restore MainScreeningData_Screen1.RData if the file becomes corrupted.

within `/code`

> `01_screening-code` contains all R files used in the intial screening. This includes `Stage 1A Set-up code.R` for quality control, `Stage 1B Auto Screen.R` (currently unused) which contains examples of code where you could automatically screen/tag different papers depending on key words, and `Stage 1C Shiny Screen.R` which does the intial manual screen using an R shiny GUI

## Systematic Review Software


An R-shiny App to make systematically reviewing easier in the Natural and Social Sciences. 

This app is still very much in development, but feel free to share and use! Let us know if you have any suggestions. 


### How to use
Working on a systematic review? Here's our current workflow. 

#### Initial screen using our example data.

1. Make sure R and R shiny are up to date (simply reinstall them: https://posit.co/download/rstudio-desktop/)

2. Open the R project via double clicking `OPEN-THIS-TO-ACCESS-PROJECT.Rproj`

3. Create your screening data by opening/running `Stage 1A Set-up code.R`.  The code isn't yet perfect, so you will have to tweak the hardwired addresses to match the folder location on your own computer.  
 
4. Then open/run the initial screening code:  `Stage 1C Shiny Screen.R`. The code isn't yet perfect, so you will have to tweak the hardwired addresses to match the folder location on your own computer.  NOTE THE GUI WILL START ON A BLANK PAGE OR OLD PAPER, CLICK NEXT UNTIL YOU GET A FRESH ABSTRACT

#### Adjusting for your own use.

We first suggest forking this repository or copying for your own use and renaming to match your interest. 

You can get the intial files via Web of Science. 

While adjusting these for your own use, take some time to think through what information and questions you'll be asking for your analysis. 
We recommend constructing a series of Yes/No statements you can ask of every abstract. Here are ours:

```
GOLD STAR. The paper is extremely relevant to our analysis and questions about the impact and vulnerability associated with flash floods
REJECT. [yes] Reject - The paper is NOT relevant to flash flooding or hydrology in general
REJECT_RAIN [yes] Reject - the paper is about water/rain/hydrology, not specifically about floods as a hazard

TOPICS
Used for initial tagging when the paper will likely be included in our analysis. 
    1. [YES] The paper is primarily geophysically focused
    2. [YES] The paper is primarily socio-politically/impact focused
    3. [YES] The paper is about one or more historical flood events
```

### Support

Something not working? Have any questions? 

Tweet us me <a href="http://twitter.com/zentouro" target="_blank">`@zentouro`</a>

### Whodunit ✨

This project exists because of these excellent folks:

<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->
<!-- prettier-ignore-start -->
<!-- markdownlint-disable -->
<table>
  <tr>
    <td align="center"><a href="http://zentouro.ldeo.columbia.edu/"><img src="https://avatars0.githubusercontent.com/u/7304202?v=4" width="100px;" alt=""/><br /><sub><b>miriam</b></sub></a><br /><a href="https://github.com/zentouro/systematic-review-flash-floods/commits?author=zentouro" title="Code">💻</a></td>
    <td align="center"><a href="https://github.com/hgreatrex"><img src="https://avatars2.githubusercontent.com/u/5038576?v=4" width="100px;" alt=""/><br /><sub><b>Helen Greatrex</b></sub></a><br /><a href="https://github.com/zentouro/systematic-review-flash-floods/commits?author=hgreatrex" title="Code">💻</a></td>
    <td align="center"><a href="https://github.com/sophielelei"><img src="https://avatars.githubusercontent.com/u/60010493?v=4" width="100px;" alt=""/><br /><sub><b>Sophie Lelei</b></sub></a><br /><a href="https://github.com/zentouro/systematic-review-flash-floods/commits?author=sophielelei" title="Code">💻</a></td>
  </tr>
</table>

<!-- markdownlint-enable -->
<!-- prettier-ignore-end -->
<!-- ALL-CONTRIBUTORS-LIST:END -->


This project follows the [all-contributors](https://github.com/all-contributors/all-contributors) specification. Contributions of any kind welcome! ([emoji key](https://allcontributors.org/docs/en/emoji-key))

Feel free to 🍴 this repo 
or <a href="https://github.com/zentouro/systematic-review-flash-floods.git" targets="_blank"> clone it to your own machine </a> 
and review to your hearts content. 

Pull requests are also welcome. 
For major changes, please open an issue first to discuss what you would like to change.


This work grew out of the [NASA GEO Forecasting Flash Flood Impact Project](https://geo.floods.global/). 



