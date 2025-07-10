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
      id: 2,
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
      id: 3,
      title:
        "Variation in sex expression and flower development in <em>Piper nigrum</em> (black pepper) assessed with micro-computed tomography and scanning electron microscopy",
      authors:
        "Wimalarathna, N.A., Von Balthazar, M., Wickramasuriya, A.M. et al.",
      journal: "Brazilian Journal of Botany",
      year: 2025,
      volume: "48",
      pages: "50",
      doi: "10.1007/s40415-025-01094-3",
      description: (
        <>
          Investigation of sex expression and floral morphology in{" "}
          <em>Piper nigrum</em> using micro-CT and SEM revealed distinct
          differences between bisexual and female flowers, with implications for
          reproductive success, fruit set, and breeding strategies.
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
