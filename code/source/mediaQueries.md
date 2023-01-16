
# **Queries for News Media Corpus Development**

**NOTE: Began by using all subject headings from [ProQuest Subject Categories - 2019-2020 Academic Year's](https://about.proquest.com/globalassets/proquest/files/pdf-files/subject-categories-academic.pdf) Environmental Sciences section (+ "Climate change", "Energy", and "Sustainability" from the Interdisciplinary section). Then, incorporated more headings upon looking at sample articles to identify additional relevant subjects such as "Environmental protection", "Environmental policy", "Global warming", "Endangered & extinct species", "Natural resources", and "Conservation".**

<br>

### **ProQuest New York Times query**

**Initial exploratory query (includes everything):**
pub.Exact("New York Times") AND
(su.Exact("Conservation biology") OR
  su.Exact("Environmental economics") OR
  su.Exact("Environmental education") OR
  su.Exact("Environmental engineering") OR
  su.Exact("Environmental geology") OR
  su.Exact("Environmental health") OR
  su.Exact("Environmental justice") OR
  su.Exact("Environmental law") OR
  su.Exact("Environmental management") OR
  su.Exact("Environmental philosophy") OR
  su.Exact("Environmental science") OR
  su.Exact("Environmental studies") OR
  su.Exact("Land use planning") OR
  su.Exact("Natural resource management") OR
  su.Exact("Water resources management") OR
  su.Exact("Wildlife conservation") OR
  su.Exact("Climate change") OR
  su.Exact("Energy") OR
  su.Exact("Sustainability") OR
  su.Exact("Environmental protection") OR
  su.Exact("Environmental policy") OR
  su.Exact("Global warming") OR
  su.Exact("Endangered & extinct species") OR
  su.Exact("Natural resources") OR
  su.Exact("Conservation"))

<br>

**Analysis**
_NOTE: the below numbers do not have a year filter, nor document type filter (i.e., still include editorials, opinion pieces, etc.)_

<br>

- 9,812 articles when only including through "Sustainability" above (to stick with subjects found within linked article highlighting ProQuest categories)
- 14,942 upon adding "Environmental protection" and "Environmental policy"
- 16,925 upon adding "Global warming"
- 18,385 upon adding "Endangered & extinct species"
- 18,605 upon adding "Natural resources"
- 19,461 upon adding "Conservation"

- 19,064 articles upon removing "Environmental economics", "Environmental education", "Environmental engineering", "Environmental geology", "Environmental health", "Environmental justice", "Environmental law", "Environmental philosophy", "Natural resource management", and "Water resources management". It seems that these topics are not typically represented on their own (i.e., in absence of one of the other above topics), only comprising ~400 articles across the full sample and timeline. We are not interested in articles that only relate to one of these subjects (e.g., Environmental education) in the absence of one of the others, so will remove them from the query.

- Two other subjects that I was _considering_ adding were: "Nature" and "Biodiversity". However, these two topics when represented on their own within articles tend to be more about art, etc. than about conservation. They don't quite fit the topic of conservation, land use planning, climate, etc. that we tried to capture in our other corpora representing the conservation sector.


**Final query for NYT:**
pub.Exact("New York Times") AND
su.Exact("Conservation biology" OR "Environmental management" OR "Environmental science" OR
         "Environmental studies" OR "Land use planning" OR "Wildlife conservation" OR
         "Climate change" OR "Energy" OR "Sustainability" OR
         "Environmental protection" OR "Environmental policy" OR "Global warming" OR
         "Endangered & extinct species" OR "Natural resources" OR "Conservation") AND
at.exact("News" OR "Feature" OR "Article") AND
pd(20000101-20211231)

NOTE: This includes additional limits of:
  - Date range: January 1 2000 to December 31 2021
  - Document type: News, Article, OR Feature

**Total number of articles (after limits applied, before any duplicates/pre-prints are removed) is 10,182.**
_Need to sort the results by "Oldest first" and then download the first 10,000 before switching to "Most recent first" to capture the final 182._

<br>

### **ProQuest Wall Street Journal query**

pub.Exact("Wall Street Journal") AND
su.Exact("Conservation biology" OR "Environmental management" OR "Environmental science" OR
         "Environmental studies" OR "Land use planning" OR "Wildlife conservation" OR
         "Climate change" OR "Energy" OR "Sustainability" OR
         "Environmental protection" OR "Environmental policy" OR "Global warming" OR
         "Endangered & extinct species" OR "Natural resources" OR "Conservation") AND
at.exact("News" OR "Feature" OR "Article") AND
pd(20000101-20211231)

NOTE: This includes additional limits of:
  - Date range: January 1 2000 to December 31 2021
  - Document type: News, Article, OR Feature

**Total number of articles (after limits applied, before any duplicates/pre-prints are removed) is 9,653.**

<br>

### **Considerations for all news media**

- Different editions (e.g., Eastern vs. European editions in WSJ)
- Distribution of different subject headings across newspapers and over time -- especially when one heading exists in the absence of others (e.g., do Energy only articles appear with more frequency in WSJ over NYT?)
- General distribution of number of articles by year, etc.
- Article format -- in WSJ in particular there are still some seemingly opinion pieces, despite filtering to News, Features, and Articles only.
- What info to keep as metadata? Year, date, subjects, author?
- Remove text on graphics (e.g., "insert graphic here"), copyrights, etc.
