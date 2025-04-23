import Head from "next/head";
import React, { useState, useEffect } from "react";
import { useRouter } from "next/router";
import Header from "../components/header";
import Footer from "../components/footer";
import styles from "../styles/gene_families.module.css";

const GeneFamilies = () => {
  const [families, setFamilies] = useState([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);
  const [activeCategory, setActiveCategory] = useState("all");
  const router = useRouter();

  // Fetch gene families from the API
  useEffect(() => {
    const fetchGeneFamilies = async () => {
      try {
        const response = await fetch("/api/geneFamilies");
        if (!response.ok) {
          throw new Error("Failed to fetch gene families");
        }
        const data = await response.json();

        // Sort families alphabetically by TF_Family
        const sortedFamilies = data.sort((a, b) =>
          a.TF_Family.localeCompare(b.TF_Family)
        );
        setFamilies(sortedFamilies);
      } catch (error) {
        setError(error.message);
      } finally {
        setLoading(false);
      }
    };

    fetchGeneFamilies();
  }, []);

  const handleFamilyClick = (familyName) => {
    const formattedFamilyName = familyName.toLowerCase().replace(/\s+/g, "-");
    router.push(`/families/${formattedFamilyName}`);
  };

  // Group families by first letter for alphabetical categorization
  const groupedFamilies = families.reduce((acc, family) => {
    const firstLetter = family.TF_Family.charAt(0).toUpperCase();
    if (!acc[firstLetter]) {
      acc[firstLetter] = [];
    }
    acc[firstLetter].push(family);
    return acc;
  }, {});

  const categories = Object.keys(groupedFamilies).sort();

  return (
    <div className={styles.appContainer}>
      <Head>
        <title>Gene Families - Black Pepper Knowledgebase</title>
        <meta
          name="description"
          content="Explore gene families of black pepper"
        />
        <link
          rel="stylesheet"
          href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css"
        />
      </Head>

      <Header />

      <main className={styles.mainContent}>
        <div className={styles.heroSection}>
          <h2>TF Families of Black Pepper</h2>
          <p className={styles.subtitle}>
            Explore the genomic diversity of <em>Piper nigrum</em>
          </p>
        </div>

        {loading && (
          <div className={styles.loadingContainer}>
            <div className={styles.loadingSpinner}></div>
            <p>Loading gene families...</p>
          </div>
        )}

        {error && (
          <div className={styles.errorMessage}>
            <i className="fas fa-exclamation-circle"></i>
            <p>Error: {error}</p>
          </div>
        )}

        {!loading && !error && (
          <div className={styles.familiesContainer}>
            <div className={styles.categoryTabs}>
              <button
                className={`${styles.categoryTab} ${
                  activeCategory === "all" ? styles.activeTab : ""
                }`}
                onClick={() => setActiveCategory("all")}
              >
                All
              </button>
              {categories.map((category) => (
                <button
                  key={category}
                  className={`${styles.categoryTab} ${
                    activeCategory === category ? styles.activeTab : ""
                  }`}
                  onClick={() => setActiveCategory(category)}
                >
                  {category}
                </button>
              ))}
            </div>

            <div className={styles.familiesGrid}>
              {(activeCategory === "all"
                ? families
                : groupedFamilies[activeCategory]
              ).map((family, index) => (
                <div
                  key={index}
                  className={styles.familyCard}
                  onClick={() => handleFamilyClick(family.TF_Family)}
                >
                  <div className={styles.familyIcon}>
                    <i className="fas fa-dna"></i>
                  </div>
                  <h3 className={styles.familyName}>{family.TF_Family}</h3>
                  {family.Gene_Count && (
                    <div className={styles.familyMeta}>
                      <span className={styles.geneCount}>
                        {family.Gene_Count} genes
                      </span>
                    </div>
                  )}
                </div>
              ))}
            </div>
          </div>
        )}
      </main>

      <Footer />
    </div>
  );
};

export default GeneFamilies;
