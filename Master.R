## Logic Package Master
library(stringr)
library(tokenizers)
library(igraph)

## 1. Base language
source("~/Axiomatica/Logic_Package/Components/Base_Language.R")
## 2. Language environments
source("~/Axiomatica/Logic_Package/Components/Language_Environments.R")
## 3. Text processing
source("~/Axiomatica/Logic_Package/Components/Text_Processor.R")
## 4. Syntax processor: constructors
source("~/Axiomatica/Logic_Package/Components/Syntax_Constructors.R")
## 5. Syntax processor: terms and formulas
source("~/Axiomatica/Logic_Package/Components/Syntax_Terms_and_Formulas.R")
## 6. Parser algorithm
source("~/Axiomatica/Logic_Package/Components/Parsing_Algorithm.R")
## 7. Syntax tests
source("~/Axiomatica/Logic_Package/Components/Syntax_Tests.R")
## 8. Parsing tree plotter
source("~/Axiomatica/Logic_Package/Components/Plot_Parse_Tree.R")
## 9. Semantics
source("~/Axiomatica/Logic_Package/Components/Semantics.R")
## 10. Tableau algorithm
source("~/Axiomatica/Logic_Package/Components/Tableau_algorithm.R")
## 11. Tableau processing
source("~/Axiomatica/Logic_Package/Components/Tableau_processing.R")
## 12. Tableau prover
source("~/Axiomatica/Logic_Package/Components/Tableau_prover.R")
## 13. Tableau plotter
source("~/Axiomatica/Logic_Package/Components/Tableau_plot.R")
