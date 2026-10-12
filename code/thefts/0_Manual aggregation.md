
## GNR

1. Given the PDFs provided by GNR
2. PDF table copied manually and pasted in CSV 
  - CSV format created with regex: 
    - ` -(\d.\d+) ` REPLACE BY `,-$1,`
    - ` -(\d.\d+)\n` REPLACE BY `,-$1,\n`
    - `,([a-z].*)\n` REPLACE BY `,"$1"\n`
    - `,(28.*)\n` REPLACE BY `,"$1"\n`
  - Added headers with first row: `"lat","lon","place"`
3. Fixed some coordinates
    - GNR south
      - 0 0 Polidesportivo da Lagoinha: 38.599021 -8.928964 (https://www.cm-palmela.pt/pages/2128?news_id=3308, https://maps.app.goo.gl/fPVcpkTeC8udwBiZ9)
      - 0 0 Rua Miraserra: 38.1742605, -8.5612147 (https://www.codigos-postal.com/setubal/grandola/grandola/rua-miraserra/)
      - 0 0 Estrada dos Espanhóis: 38.6078573, -8.8589539 (https://codigos-postal.com///setubal/palmela/pinhal-novo/estrada-dos-espanhois/)
