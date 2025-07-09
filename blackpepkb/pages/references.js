import React, { useState } from "react";
import Head from "next/head";
import styles from "../styles/references.module.css";
import Header from "../components/header";
import Footer from "../components/footer";

const ReferencesPage = () => {
  const [searchTerm, setSearchTerm] = useState("");

  const references = [
    {
      id: 1,
      title:
        "The chromosome-scale reference genome of black pepper provides insight into piperine biosynthesis",
      authors: "Hu L, Xu Z, et al.",
      journal: "Nature Communications",
      year: 2019,
      volume: "10",
      pages: "4702",
      doi: "10.1038/s41467-019-12607-6",
      description: (
        <>
          First chromosome-scale reference genome for black pepper (
          <em>Piper nigrum</em>), revealing insights into piperine biosynthesis
          pathways and evolutionary history.
        </>
      ),
    },
    {
      id: 2,
      title:
        "A comprehensive catalog of single nucleotide polymorphisms (SNPs) from the black pepper (<em>Piper nigrum</em> L.) genome",
      authors: "Thanthirige HA, Wimalarathna NA, et al.",
      journal: "BMC Genomics",
      year: 2025,
      volume: "26",
      pages: "256",
      doi: "10.1186/s12864-025-09876-w",
      description: (
        <>
          Extensive catalog of SNP markers in <em>Piper nigrum</em> genome,
          providing valuable resources for genetic diversity studies and
          breeding programs.
        </>
      ),
    },
    {
      id: 3,
      title:
        "Genetic diversity and population structure of <em>Piper nigrum</em> (black pepper) accessions based on next-generation SNP markers",
      authors: "Wimalarathna NA, Wickramasuriya AM, et al.",
      journal: "PLoS ONE",
      year: 2024,
      volume: "19",
      pages: "e0305990",
      doi: "10.1371/journal.pone.0305990",
      description: (
        <>
          Analysis of genetic diversity in <em>Piper nigrum</em> germplasm using
          SNP markers, revealing population structure and relationships among
          global accessions.
        </>
      ),
    },
    {
      id: 4,
      title:
        "Molecular characterization of black pepper (<em>Piper nigrum</em>) using RAPD and SSR markers",
      authors: "Raghavan R, Elumalai S, et al.",
      journal: "Biosciences Biotechnology Research Asia",
      year: 2010,
      volume: "7",
      pages: "1011-1015",
      doi: "10.13005/bbra/782",
      description: (
        <>
          Early molecular characterization of <em>Piper nigrum</em> varieties
          using RAPD and SSR marker systems to establish genetic relationships.
        </>
      ),
    },
    {
      id: 5,
      title:
        "Chemical composition, nutritional, medicinal and functional properties of black pepper: a review",
      authors: "Meghwal M, Goswami TK",
      journal: "Open Access Scientific Reports",
      year: 2012,
      volume: "1",
      pages: "1-5",
      doi: "10.4172/scientificreports.172",
      description: (
        <>
          Comprehensive review of <em>Piper nigrum</em>&apos;s chemical
          components, nutritional value, and medicinal properties, with focus on
          bioactive compounds.
        </>
      ),
    },
    {
      id: 6,
      title:
        "Drought responsiveness in black pepper (<em>Piper nigrum</em> L.): genes associated and development of a web-genomic resource",
      authors: "Negi A, Kokkat JG, et al.",
      journal: "Physiologia Plantarum",
      year: 2020,
      volume: "172",
      pages: "669-683",
      doi: "10.1111/ppl.13208",
      description: (
        <>
          Identification of drought-responsive genes in <em>Piper nigrum</em>{" "}
          and development of a web-based genomic resource for investigating
          stress responses.
        </>
      ),
    },
    {
      id: 7,
      title: "Black pepper",
      authors: "Ravindran PN, Kallupurackal JA",
      journal: "Handbook of Herbs and Spices",
      year: 2012,
      volume: "2nd ed.",
      pages: "86-115",
      doi: "10.1533/9780857095671.86",
      description: (
        <>
          Comprehensive overview of <em>Piper nigrum</em> cultivation,
          processing, quality parameters, and global trade perspectives in the
          herbs and spices handbook.
        </>
      ),
    },
    {
      id: 8,
      title:
        "Agronomy and Economy of Black Pepper and Cardamom: The 'king' and 'queen' of Spices",
      authors: "Nair KPP",
      journal: "Elsevier",
      year: 2011,
      volume: "1st ed.",
      pages: "1-366",
      doi: "10.1016/C2010-0-65636-2",
      description: (
        <>
          Definitive text on <em>Piper nigrum</em> agronomy, production systems,
          economic aspects, and global market dynamics with historical context.
        </>
      ),
    },
    {
      id: 9,
      title:
        "MagnoliidsGDB: An integrated functional genomics database for magnoliids",
      authors: "Chen Y, Yang Z, et al.",
      journal: "Plant Communications",
      year: 2025,
      volume: "",
      pages: "101263",
      doi: "10.1016/j.plcom.2025.101263",
      description: (
        <>
          Database resource integrating genomic, transcriptomic, and functional
          data for magnoliids including <em>Piper nigrum</em>.
        </>
      ),
    },
    {
      id: 10,
      title: "PGD: pineapple genomics database",
      authors: "Xu H, Yu Q, et al.",
      journal: "Horticulture Research",
      year: 2018,
      volume: "5",
      pages: "66",
      doi: "10.1038/s41438-018-0078-2",
      description: (
        <>
          Genomic database structure and design serving as model for development
          of similar resources for tropical crops like <em>Piper nigrum</em>.
        </>
      ),
    },
    {
      id: 11,
      title:
        "JBrowse 2: A modular genome browser with views of synteny and structural variation",
      authors: "Diesh C, Stevens GJ, et al.",
      journal: "Genome Biology",
      year: 2023,
      volume: "24",
      pages: "74",
      doi: "10.1186/s13059-023-02896-y",
      description: (
        <>
          Next-generation genome browser platform used for visualizing{" "}
          <em>Piper nigrum</em> genomic data and structural features.
        </>
      ),
    },
    {
      id: 12,
      title: "BLAST+: Architecture and applications",
      authors: "Camacho C, Coulouris G, et al.",
      journal: "BMC Bioinformatics",
      year: 2009,
      volume: "10",
      pages: "421",
      doi: "10.1186/1471-2105-10-421",
      description: (
        <>
          Core sequence alignment tool used in <em>Piper nigrum</em> genome
          annotation and comparative genomic analyses.
        </>
      ),
    },
    {
      id: 13,
      title: "AmiGO: Online access to ontology and annotation data",
      authors: "Carbon S, Ireland A, et al.",
      journal: "Bioinformatics",
      year: 2009,
      volume: "25",
      pages: "288-289",
      doi: "10.1093/bioinformatics/btn615",
      description: (
        <>
          Gene Ontology browser utilized for functional annotation of{" "}
          <em>Piper nigrum</em> genes and pathways.
        </>
      ),
    },
    {
      id: 14,
      title:
        "Blast2GO: A universal tool for annotation, visualization and analysis in functional genomics research",
      authors: "Conesa A, Götz S, et al.",
      journal: "Bioinformatics",
      year: 2005,
      volume: "21",
      pages: "3674-3676",
      doi: "10.1093/bioinformatics/bti610",
      description: (
        <>
          Annotation platform employed for functional characterization of{" "}
          <em>Piper nigrum</em> genomic and transcriptomic data.
        </>
      ),
    },
    {
      id: 15,
      title:
        "An 'electronic fluorescent pictograph' browser for exploring and analyzing large-scale biological data sets",
      authors: "Winter D, Vinegar B, et al.",
      journal: "PLoS One",
      year: 2007,
      volume: "2",
      pages: "e718",
      doi: "10.1371/journal.pone.0000718",
      description: (
        <>
          Expression data visualization tool adapted for exploring{" "}
          <em>Piper nigrum</em> tissue-specific gene expression patterns.
        </>
      ),
    },
    {
      id: 16,
      title:
        "Maizegdb 2018: the maize multi-genome genetics and genomics database",
      authors: "Portwood JL, Woodhouse MR, et al.",
      journal: "Nucleic Acids Research",
      year: 2018,
      volume: "47",
      pages: "D1146-D1154",
      doi: "10.1093/nar/gky1046",
      description: (
        <>
          Model crop database architecture providing framework for{" "}
          <em>Piper nigrum</em> genomic database development.
        </>
      ),
    },
  ];

  // Filter references based on search term
  const filteredReferences = references.filter((ref) => {
    const searchContent = `${ref.title} ${ref.authors} ${
      typeof ref.description === "string"
        ? ref.description
        : ref.description.props.children.join(" ")
    }`.toLowerCase();

    return (
      searchTerm === "" || searchContent.includes(searchTerm.toLowerCase())
    );
  });

  return (
    <div className={styles.pageWrapper}>
      <Head>
        <title>References | BlackPepKB</title>
        <link
          rel="stylesheet"
          href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"
        />
        <meta
          name="description"
          content="Comprehensive collection of scientific references and resources for black pepper research"
        />
      </Head>

      <Header />

      <main className={styles.mainContainer}>
        <section className={styles.heroSection}>
          <div className={styles.heroContent}>
            <h1 className={styles.heroTitle}>References</h1>
          </div>
        </section>

        <section className={styles.referencesSection}>
          <div className={styles.containerInner}>
            <div className={styles.filterControls}>
              <div className={styles.searchBox}>
                <i className="fa-solid fa-magnifying-glass"></i>
                <input
                  type="text"
                  placeholder="Search references..."
                  value={searchTerm}
                  onChange={(e) => setSearchTerm(e.target.value)}
                  className={styles.searchInput}
                />
                {searchTerm && (
                  <button
                    className={styles.clearButton}
                    onClick={() => setSearchTerm("")}
                  >
                    <i className="fa-solid fa-times"></i>
                  </button>
                )}
              </div>
            </div>

            <div className={styles.referencesList}>
              {filteredReferences.length > 0 ? (
                filteredReferences.map((ref) => (
                  <div key={ref.id} className={styles.referenceCard}>
                    <div className={styles.referenceHeader}>
                      <span className={styles.referenceYear}>{ref.year}</span>
                    </div>
                    <h3 className={styles.referenceTitle}>
                      {ref.title.includes("<em>") ? (
                        <span dangerouslySetInnerHTML={{ __html: ref.title }} />
                      ) : (
                        ref.title
                      )}
                    </h3>
                    <p className={styles.referenceAuthors}>{ref.authors}</p>
                    <p className={styles.referenceJournal}>
                      <em>{ref.journal}</em>, {ref.volume}, {ref.pages}
                    </p>
                    <div className={styles.referenceDescription}>
                      {ref.description}
                    </div>
                    <div className={styles.referenceLinks}>
                      <a
                        href={`https://doi.org/${ref.doi}`}
                        target="_blank"
                        rel="noopener noreferrer"
                        className={styles.doiLink}
                      >
                        <i className="fa-solid fa-external-link-alt"></i> DOI:{" "}
                        {ref.doi}
                      </a>
                    </div>
                  </div>
                ))
              ) : (
                <div className={styles.noResults}>
                  <i className="fa-solid fa-search"></i>
                  <p>No references found matching your search criteria</p>
                  <button
                    className={styles.resetButton}
                    onClick={() => {
                      setSearchTerm("");
                    }}
                  >
                    Reset Search
                  </button>
                </div>
              )}
            </div>
          </div>
        </section>
      </main>

      <Footer />
    </div>
  );
};

export default ReferencesPage;
