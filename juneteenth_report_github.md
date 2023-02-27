Juneteenth
================
Sandro Lelis
2023-02-27

## National Lynching, 1883-1941 (Codebook)

<br>

These data were collected by Charles Seguin and David Rigby. They
supplement existing lynching inventories to cover the contiguous United
States. Details are covered in Seguin and Rigby (2019). Please cite as
follows:<br>

Seguin, Charles and David Rigby. 2019. National Crimes: A New National
Data Set of Lynchings in the United States, 1883 to 1941. Socius
5:2378023119841780.

<br>

### About Juneteenth

<br>

<p>

Juneteenth is a federal holiday in the United States commemorating the
emancipation of enslaved African Americans. Deriving its name from
combining “June” and “nineteenth”, it is celebrated on the anniversary
of General Order No. 3, issued by Major General Gordon Granger on June
19, 1865, proclaiming freedom for slaves in Texas.\[7\] Originating in
Galveston, Juneteenth has since been observed annually in various parts
of the United States, often broadly celebrating African-American
culture. The day was first recognized as a federal holiday in 2021, when
President Joe Biden signed the Juneteenth National Independence Day Act
into law after the efforts of Lula Briggs Galloway, Opal Lee, and
others.

<p>

Early celebrations date to 1866, at first involving church-centered
community gatherings in Texas. They spread across the South and became
more commercialized in the 1920s and 1930s, often centering on a food
festival. Participants in the Great Migration brought these celebrations
to the rest of the country. During the Civil Rights Movement of the
1960s, these celebrations were eclipsed by the nonviolent determination
to achieve civil rights, but grew in popularity again in the 1970s with
a focus on African American freedom and African-American arts. Beginning
with Texas by proclamation in 1938, and by legislation in 1979, every
U.S. state and the District of Columbia has formally recognized the
holiday in some way. With its adoption in parts of Mexico, the holiday
has become an international holiday. Juneteenth is celebrated by the
Mascogos, descendants of Black Seminoles who escaped from slavery in
1852 and settled in Coahuila, Mexico.

<p>

Celebratory traditions often include public readings of the Emancipation
Proclamation, singing traditional songs such as “Swing Low, Sweet
Chariot” and “Lift Every Voice and Sing”, and the reading of works by
noted African-American writers, such as Ralph Ellison and Maya Angelou.
Juneteenth celebrations may also include rodeos, street fairs, cookouts,
family reunions, parties, historical reenactments, and Miss Juneteenth
contests. In 2021, Juneteenth became the first new federal holiday since
Martin Luther King Jr. Day was adopted in 1983.

<p>

Source: Wikipedia

<br> In this project we will look for some patterns shown in details on
this data, to have a realistic view on what this storytelling is about.
Notice that this dataset is populated by victims that was possible to be
confirmed with a valid document, this mean that not all victims are
included in this dataset.

<br>

### Loading Dataset

<br>

``` r
state_lookup <- tibble(abb = state.abb,
       name = state.name) # convert state names.

data2 <- read_delim("Data.csv", delim = ";", 
                   escape_double = FALSE, trim_ws = TRUE)
```

    ## Rows: 1328 Columns: 18
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ";"
    ## chr (15): caseid, full_fips, state, state_fips, county, county_fips, city, v...
    ## dbl  (3): year, month, day
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

<br>

### Cleaning Data

<br>

``` r
colSums(is.na(data2)) # there is NA values.
```

    ##              caseid                year               month                 day 
    ##                   0                   0                   0                   0 
    ##           full_fips               state          state_fips              county 
    ##                   0                   0                   0                   0 
    ##         county_fips                city              victim              gender 
    ##                   0                   5                   0                   0 
    ##                race     alleged_offense        lynch_method  composition_of_mob 
    ##                   0                   6                   0                   9 
    ##    source_of_record confirming_document 
    ##                  12                  61

``` r
sum(duplicated(data2)) # there isn't duplicated values.
```

    ## [1] 0

<br>

### Data Manipulation

<br>

``` r
lynchings_per_state_per_year <- data2 %>% 
        filter(race == "Black") %>% 
        count(year, state) %>% 
        inner_join(., state_lookup, by = c("state" = "abb")) %>% 
        na.omit() # Looking at Lynchings per State and per Year.

colSums(is.na(lynchings_per_state_per_year)) # Checking for NA values.
```

    ##  year state     n  name 
    ##     0     0     0     0

``` r
datatable(lynchings_per_state_per_year) 
```

<div class="datatables html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-99332d75f70fd00bc3e6" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-99332d75f70fd00bc3e6">{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187","188","189","190","191","192","193","194","195","196","197","198","199","200","201","202","203","204","205","206","207","208","209","210","211","212","213","214","215","216","217","218","219","220","221","222","223","224","225","226","227","228","229","230","231","232","233","234","235","236","237","238"],[1883,1883,1883,1883,1883,1884,1884,1884,1884,1884,1885,1885,1885,1885,1885,1885,1885,1885,1886,1886,1886,1886,1886,1886,1886,1887,1887,1887,1887,1887,1887,1887,1887,1887,1888,1888,1888,1888,1888,1888,1888,1889,1889,1889,1889,1889,1889,1889,1889,1890,1890,1890,1890,1890,1890,1891,1891,1891,1891,1891,1891,1891,1892,1892,1892,1892,1892,1892,1892,1892,1893,1893,1893,1893,1893,1894,1894,1894,1894,1894,1894,1894,1894,1894,1895,1895,1895,1895,1896,1896,1896,1896,1897,1897,1897,1897,1897,1898,1898,1898,1898,1898,1898,1898,1899,1899,1899,1899,1900,1900,1900,1900,1900,1900,1900,1901,1901,1901,1901,1901,1901,1901,1902,1902,1902,1902,1902,1902,1902,1902,1903,1903,1903,1903,1904,1904,1904,1904,1904,1905,1905,1905,1906,1906,1906,1906,1907,1907,1907,1908,1908,1909,1909,1909,1910,1910,1910,1910,1911,1911,1911,1911,1911,1911,1912,1912,1912,1912,1913,1913,1913,1913,1914,1914,1914,1915,1915,1915,1916,1916,1916,1917,1917,1917,1917,1918,1918,1918,1918,1919,1919,1919,1920,1920,1920,1920,1920,1920,1921,1921,1921,1922,1922,1923,1923,1923,1923,1924,1924,1924,1925,1925,1925,1926,1926,1927,1927,1927,1927,1928,1928,1929,1930,1930,1930,1931,1931,1931,1932,1932,1932,1933,1933,1933,1934,1935,1936,1936],["IL","IN","MO","TX","UT","MD","MO","NM","TX","VA","KS","MD","MO","OH","SD","TX","VA","WV","IN","MD","NJ","OK","TX","VA","WV","CO","KS","MD","NE","OH","OK","TX","VA","WV","CO","IL","KS","MT","TX","VA","WV","IN","KS","MI","MO","TN","TX","VA","WV","AR","IN","MO","OK","TX","VA","IN","MD","MO","NE","TX","VA","WV","KS","MD","MO","NY","OH","TX","VA","WV","IL","KS","MO","TX","VA","KS","MD","MO","OH","PA","TN","TX","VA","WV","MD","MO","OH","TX","MD","OK","TX","WV","MD","MO","OH","TX","VA","IL","MD","MO","OK","TX","VA","WV","KS","MO","TX","VA","CO","IN","MD","MO","TX","VA","WV","IN","KS","MO","OK","TX","VA","WV","CO","IN","KS","MO","OR","TX","VA","WV","DE","IL","MO","TX","CA","OH","TX","VA","WY","MO","TX","VA","MD","MO","OK","TX","MD","OK","TX","IL","TX","IL","OK","TX","MO","TX","VA","WV","MD","MO","OH","OK","PA","TX","KS","TX","WV","WY","IL","MT","OK","TX","MO","OK","TX","MO","OK","TX","MO","OK","TX","OK","TX","VA","WY","OK","TX","VA","WY","MO","NE","TX","KS","MN","MO","OK","TX","VA","MO","TX","VA","OK","TX","MO","OK","TX","VA","IL","MO","TX","MO","UT","VA","TX","VA","CA","MO","TX","VA","MO","TX","TX","IN","OK","TX","MD","MO","WV","KS","OH","TX","MD","MO","TX","TX","TX","OK","TX"],[1,1,1,7,1,1,2,1,8,3,2,3,1,2,1,15,3,1,1,1,1,2,16,2,1,1,2,1,1,1,1,6,1,3,1,2,2,1,3,3,1,1,2,1,2,1,6,7,3,1,1,2,1,9,3,1,1,1,1,8,5,1,3,1,1,1,1,8,5,4,1,2,2,2,10,1,2,3,2,1,1,4,1,1,2,2,1,17,2,1,6,1,1,1,1,19,1,1,2,3,1,2,1,1,2,2,4,1,2,3,1,2,3,4,2,1,1,6,1,5,2,1,1,1,1,3,1,6,4,3,1,3,2,6,1,1,3,3,1,2,9,1,1,4,2,7,2,4,4,2,15,1,1,12,3,5,1,1,1,2,1,5,1,3,1,3,1,1,2,1,5,6,1,3,4,1,1,4,1,4,7,1,5,2,1,1,8,1,1,1,1,2,1,3,1,2,9,2,1,6,1,1,15,1,2,1,1,1,1,1,1,1,1,3,1,1,1,1,1,1,2,2,2,1,4,1,1,2,1,1,1,1,1,1,1,2,1,1],["Illinois","Indiana","Missouri","Texas","Utah","Maryland","Missouri","New Mexico","Texas","Virginia","Kansas","Maryland","Missouri","Ohio","South Dakota","Texas","Virginia","West Virginia","Indiana","Maryland","New Jersey","Oklahoma","Texas","Virginia","West Virginia","Colorado","Kansas","Maryland","Nebraska","Ohio","Oklahoma","Texas","Virginia","West Virginia","Colorado","Illinois","Kansas","Montana","Texas","Virginia","West Virginia","Indiana","Kansas","Michigan","Missouri","Tennessee","Texas","Virginia","West Virginia","Arkansas","Indiana","Missouri","Oklahoma","Texas","Virginia","Indiana","Maryland","Missouri","Nebraska","Texas","Virginia","West Virginia","Kansas","Maryland","Missouri","New York","Ohio","Texas","Virginia","West Virginia","Illinois","Kansas","Missouri","Texas","Virginia","Kansas","Maryland","Missouri","Ohio","Pennsylvania","Tennessee","Texas","Virginia","West Virginia","Maryland","Missouri","Ohio","Texas","Maryland","Oklahoma","Texas","West Virginia","Maryland","Missouri","Ohio","Texas","Virginia","Illinois","Maryland","Missouri","Oklahoma","Texas","Virginia","West Virginia","Kansas","Missouri","Texas","Virginia","Colorado","Indiana","Maryland","Missouri","Texas","Virginia","West Virginia","Indiana","Kansas","Missouri","Oklahoma","Texas","Virginia","West Virginia","Colorado","Indiana","Kansas","Missouri","Oregon","Texas","Virginia","West Virginia","Delaware","Illinois","Missouri","Texas","California","Ohio","Texas","Virginia","Wyoming","Missouri","Texas","Virginia","Maryland","Missouri","Oklahoma","Texas","Maryland","Oklahoma","Texas","Illinois","Texas","Illinois","Oklahoma","Texas","Missouri","Texas","Virginia","West Virginia","Maryland","Missouri","Ohio","Oklahoma","Pennsylvania","Texas","Kansas","Texas","West Virginia","Wyoming","Illinois","Montana","Oklahoma","Texas","Missouri","Oklahoma","Texas","Missouri","Oklahoma","Texas","Missouri","Oklahoma","Texas","Oklahoma","Texas","Virginia","Wyoming","Oklahoma","Texas","Virginia","Wyoming","Missouri","Nebraska","Texas","Kansas","Minnesota","Missouri","Oklahoma","Texas","Virginia","Missouri","Texas","Virginia","Oklahoma","Texas","Missouri","Oklahoma","Texas","Virginia","Illinois","Missouri","Texas","Missouri","Utah","Virginia","Texas","Virginia","California","Missouri","Texas","Virginia","Missouri","Texas","Texas","Indiana","Oklahoma","Texas","Maryland","Missouri","West Virginia","Kansas","Ohio","Texas","Maryland","Missouri","Texas","Texas","Texas","Oklahoma","Texas"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>year<\/th>\n      <th>state<\/th>\n      <th>n<\/th>\n      <th>name<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[1,3]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>

<br>

``` r
lynchings_per_state <- lynchings_per_state_per_year %>% 
        group_by(name) %>% 
        summarize(N = sum(n)) %>% 
        mutate(state = glue("{name} ({N})")) # Looking for Lynchings per State and summarize the result.
```

<br>

``` r
datatable(lynchings_per_state)
```

<div class="datatables html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-15f3f16e30eddae571f1" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-15f3f16e30eddae571f1">{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27"],["Arkansas","California","Colorado","Delaware","Illinois","Indiana","Kansas","Maryland","Michigan","Minnesota","Missouri","Montana","Nebraska","New Jersey","New Mexico","New York","Ohio","Oklahoma","Oregon","Pennsylvania","South Dakota","Tennessee","Texas","Utah","Virginia","West Virginia","Wyoming"],[1,2,5,1,14,12,21,24,1,3,60,2,3,1,1,1,11,41,1,2,1,2,307,2,71,27,4],["Arkansas (1)","California (2)","Colorado (5)","Delaware (1)","Illinois (14)","Indiana (12)","Kansas (21)","Maryland (24)","Michigan (1)","Minnesota (3)","Missouri (60)","Montana (2)","Nebraska (3)","New Jersey (1)","New Mexico (1)","New York (1)","Ohio (11)","Oklahoma (41)","Oregon (1)","Pennsylvania (2)","South Dakota (1)","Tennessee (2)","Texas (307)","Utah (2)","Virginia (71)","West Virginia (27)","Wyoming (4)"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>name<\/th>\n      <th>N<\/th>\n      <th>state<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":2},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>

<br>

### Plotting Deadly States & Countys

<br>

``` r
lynchings_per_state_per_year %>% 
        inner_join(., lynchings_per_state, by = "name") %>%
        arrange(N) %>% 
        ggplot(aes(x=year, fill=n, y=name))+
        geom_tile()+
        scale_fill_gradient(name = "Number of\nLynchings", low = "#FFFFFF", high = "#FF0000",
                            limits=c(0, NA))+
        labs(x="Year", y=NULL, title = glue("Between 1883-1941 Texas had the most Lynchings"))+
        theme_classic()+
        theme(axis.line = element_blank(),
              axis.ticks = element_blank(),
              plot.title = element_textbox_simple(size = 20, face = "bold",
                                                   margin = margin(t=10, 0, b=15, 0)),
        plot.title.position = "plot")
```

![](juneteenth_report_github_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

<br>

### Data Manipulation

<br>

``` r
lynchings_per_year <- data2 %>% 
        filter(race == "Black") %>% 
        count(year) %>% 
        na.omit() # Looking Lynching through time.
```

<br>

``` r
datatable(lynchings_per_year)
```

<div class="datatables html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-5d03180327e5e48755bb" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-5d03180327e5e48755bb">{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54"],[1883,1884,1885,1886,1887,1888,1889,1890,1891,1892,1893,1894,1895,1896,1897,1898,1899,1900,1901,1902,1903,1904,1905,1906,1907,1908,1909,1910,1911,1912,1913,1914,1915,1916,1917,1918,1919,1920,1921,1922,1923,1924,1925,1926,1927,1928,1929,1930,1931,1932,1933,1934,1935,1936],[11,15,28,24,17,13,23,17,18,24,17,16,22,10,23,11,9,17,17,20,12,9,12,14,10,17,14,10,13,6,14,8,6,12,9,11,4,18,8,16,5,3,3,4,4,3,2,7,4,3,3,1,2,2]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>year<\/th>\n      <th>n<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[1,2]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>

<br>

``` r
year_range <- lynchings_per_year %>% 
        summarize(early = min(year), late = max(year)) %>% 
        mutate(range = glue("{early} and {late}")) %>% 
        pull(range) # Look at Lynchings year range.

peak_year <- lynchings_per_year %>% 
        top_n(n,n=1) %>% 
        pull(year) # Pull top year.

total_lynchings <- lynchings_per_year %>% 
        summarize(N = sum(n)) # Total Lynchings.
```

<br>

### Plotting Year Range

``` r
lynchings_per_year %>% 
        ggplot(aes(x=year, y=n))+
        geom_line()+
        labs(x="Year", y="Number of Lynchings",
             title = glue("Between {year_range} there were {total_lynchings} Black individuals lynchings with the peak accuring in {peak_year}"))+
        theme_classic()+
        theme(plot.title = element_textbox_simple(size = 20, face = "bold",
                                                  margin = margin(t=10, 0, b=15, 0)),
              plot.title.position = "plot")
```

![](juneteenth_report_github_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

<br>

### Rate of Lynchings by Race

<br>

### Descriptive Analysis:

<br>

Despite of the unaccurated dataset, this dataset lead us to conclude
that there were significant lychings for white race individuals, mostly
related to outlaws, as we can observe by the circunstaces of the
lychings mostly with a mob composition by “masked men” revealing gang
rivals, “vigilance commitee” revealing lynchings for crime comitted and
escape, “party of texans” revealing rivalry and “band of indians” .

In this analysis it was also excluded white hispanic that were refered
as white race individuals. Although it was noticed that black
individuals suffered the most deaths by lynching in this dataset, we can
observe in the clustering plot that black individuals populate deaths
from cluster 2 to cluster 5, while the remaining races were grouped in
cluster 1 wich have the lowest death rates, and most didn’t had any
allegation, mostly by hangging and shot.
