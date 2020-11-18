FROM bioshinybase_18nov2020
RUN mkdir -p /home/app
RUN mkdir /home/app/pkgs
WORKDIR /home/app

#--------------------------------------------------------------------------------
# run installPrerequisites only if the base build was incomplete
# skip if now, and most of the time
#--------------------------------------------------------------------------------
COPY ./installPrerequisites.R .
RUN R -f ./installPrerequisites.R .

COPY pkgs/* ./pkgs/

RUN R CMD INSTALL pkgs/msgBoxWidget_1.0.0.tar.gz
RUN R CMD INSTALL pkgs/dataTableWidget_1.0.10.tar.gz
RUN R CMD INSTALL pkgs/GOEnrichmentWidget_1.0.5.tar.gz
RUN R CMD INSTALL pkgs/HeatmapWidget_2.0.2.tar.gz
RUN R CMD INSTALL pkgs/igvShiny_1.2.6.tar.gz
RUN R CMD INSTALL pkgs/igvWidget_1.0.9.tar.gz
RUN R CMD INSTALL pkgs/GenomeTracksWidget_1.0.3.tar.gz
