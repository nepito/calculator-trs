FROM islasgeci/base:0.7.0
COPY . /workdir
RUN Rscript -e "install.packages(c('covr', 'devtools', 'lintr', 'roxygen2', 'styler', 'testthat'), repos='http://cran.rstudio.com')"
RUN Rscript -e "install.packages(c('comprehenr', 'ggpubr', 'jsonify', 'optparse', 'plotly', 'tidyverse'), repos='http://cran.rstudio.com')"
RUN curl -fsSL https://get.deta.dev/cli.sh | sh
CMD ["make"]
