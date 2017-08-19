FROM r-base
RUN mkdir -p /Data
COPY Metabric_Web_Scraping.R / 

RUN Rscript -e "install.packages('cgdsr')"

CMD ["Rscript","Metabric_Web_Scraping.R"]