  window.dalliance_br = new Browser({
      chr:        '19',
      viewStart:  30000000,
      viewEnd:    30030000,
      cookieKey:  'mouse',

      coordSystem: {
          speciesName: 'Mouse',
          taxon: 10090,
          auth: 'NCBIM',
          version: 37
      },

    chains: {
      mm8ToMm9: new Chainset('http://www.derkholm.net:8080/das/mm8ToMm9/', 'NCBIM36', 'NCBIM37',
                               {
                                  speciesName: 'Mouse',
                                  taxon: 10090,
                                  auth: 'NCBIM',
                                  version: 36
                               })
    },


      sources:      [{name: 'Genome',
                     uri:  'http://www.derkholm.net:9080/das/mm9comp/',
                     desc: 'Mouse reference genome build NCBIm37',
                     tier_type: 'sequence',
                     provides_entrypoints: true},
                    {name: 'Genes',
                     desc: 'Gene structures from Ensembl 58',
                     uri:  'http://www.derkholm.net:8080/das/mmu_58_37k/',
                     collapseSuperGroups: true,
                     provides_karyotype: true,
                     provides_search: true},
                    {name: 'Repeats',
                     desc: 'Repeat annotation from Ensembl 58',
                     uri: 'http://www.derkholm.net:8080/das/mmu_58_37k/',
                     stylesheet_uri: 'http://www.derkholm.net/dalliance-test/stylesheets/mouse-repeats.xml'},
                    {name: 'CpG',
                     desc: 'CpG observed/expected ratio',
                     uri: 'http://www.derkholm.net:9080/das/mm9comp/',
                     stylesheet_uri: 'http://www.derkholm.net/dalliance-test/stylesheets/cpg.xml'}],

 
    searchEndpoint: new DASSource('http://www.derkholm.net:8080/das/mmu_58_37k/'),
    karyoEndpoint: new DASSource('http://www.derkholm.net:8080/das/mmu_58_37k/'),
 
    browserLinks: {
        Ensembl: 'http://www.ensembl.org/Mus_musculus/Location/View?r=${chr}:${start}-${end}',
        UCSC: 'http://genome.ucsc.edu/cgi-bin/hgTracks?db=mm9&position=chr${chr}:${start}-${end}'
    }
});
