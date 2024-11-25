# Text_Mining_Peru_Goverment
# Planes de Gobierno - Perú 2021: A Text Analysis

## Repository Overview

This repository contains the RMarkdown project `Planes de Gobierno - Perú 2021: Un análisis de texto`, which presents a detailed text mining analysis of the government plans for the 2021 Peruvian presidential elections. The project focuses on the six leading political parties based on voter intention as reported by [IPSOS](https://www.ipsos.com/sites/default/files/ct/news/documents/2021-01/anexo1_publicacion.pdf). These parties are:

- Victoria Nacional
- Fuerza Popular
- Partido Morado
- Juntos por el Perú
- Podemos Perú
- Acción Popular

The primary goal is to provide an impartial understanding of the key themes, sentiments, and differentiators across these government plans.

---

## Features

The analysis includes:
1. **Global Word Frequency**: Understanding the most commonly used words in all documents.
2. **Sentiment Analysis**: Classifying and comparing the positive and negative sentiments expressed.
3. **TF-IDF Analysis**: Identifying unique and significant words for each party's plan.
4. **Bigram Analysis**: Exploring word combinations to uncover hidden insights.
5. **Correlation Analysis**: Comparing similarities between parties based on word frequencies.
6. **Visualization**: Word clouds, bar charts, and other visualizations to present findings.

---

## Tools and Libraries

The project employs the following tools and libraries:

- **Text Mining and Preprocessing**: `tidytext`, `tidyverse`, `reshape2`
- **Visualization**: `ggplot2`, `wordcloud`, `wordcloud2`, `GGally`, `ggraph`
- **Sentiment Analysis**: Custom sentiment dictionary loaded from `dictionary.xlsx`
- **Other Utilities**: `igraph`, `widyr`, `scales`, `RColorBrewer`, `rmdformats::robobook` for enhanced HTML output.

---

## Methodology

1. **Text Preprocessing**:
   - The plans of government were tokenized using `unnest_tokens` to split text into words.
   - Stop words and context-specific irrelevant terms (e.g., "año", "meta", "mil") were removed.

2. **Sentiment Analysis**:
   - Words were matched against a custom sentiment dictionary to classify as positive or negative.
   - Sentiment scores were calculated as:
     \[
     \text{Score} = \frac{\text{Positive Words} - \text{Negative Words}}{\text{Total Words}}
     \]

3. **TF-IDF Analysis**:
   - The importance of terms was evaluated using the Term Frequency-Inverse Document Frequency (TF-IDF) methodology to highlight terms unique to each party.

4. **Bigram Analysis**:
   - Two-word combinations (bigrams) were extracted and analyzed for contextual insights.

5. **Correlation Analysis**:
   - Similarities between parties were computed using word proportions and displayed in a correlation matrix.

---

## Key Findings

- **Common Themes**: Words such as "salud" (health) and "desarrollo" (development) were prevalent across all plans, reflecting concerns about the COVID-19 pandemic and economic recovery.
- **Sentiment Variations**: Some parties, like Partido Morado and Juntos por el Perú, leaned towards positive language, while others, like Podemos and Acción Popular, emphasized challenges and negative sentiments.
- **Party-Specific Insights**:
  - Acción Popular highlighted pension reforms.
  - Fuerza Popular focused on social assistance.
  - Juntos por el Perú emphasized minority rights and inclusion.
  - Partido Morado incorporated innovative themes like the fourth industrial revolution.
  - Podemos prioritized tax relief and remote services.
  - Victoria Nacional underscored judicial reforms and infrastructure projects.

---

## Visualizations

- Word clouds for common and party-specific terms.
- Bar plots of word frequencies and sentiment contributions.
- TF-IDF comparisons highlighting unique keywords per party.
- Correlation heatmaps between parties based on lexical similarities.

---

## How to Use

1. Clone the repository:
   ```bash
   git clone https://github.com/username/planes-gobierno-peru-2021.git
   ```
2. Install the required R libraries:
   ```R
   install.packages(c("tidyverse", "tidytext", "ggplot2", "wordcloud", "ggraph", "igraph", "reshape2", "scales", "RColorBrewer"))
   ```
3. Run the RMarkdown file to generate the analysis:
   ```R
   rmarkdown::render("planes_gobierno_peru_2021.Rmd")
   ```

---

## Author

Luis Jose Zapata Bobadilla  
Graduate Student in Quantitative Economics, UCLA  

For questions or collaboration, contact: [luisjo819@g.ucla.edu](mailto:luisjo819@g.ucla.edu)

---

## License

This project is licensed under the MIT License. See the LICENSE file for details.
