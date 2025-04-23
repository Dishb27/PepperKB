import React from "react";
import Head from "next/head";
import Link from "next/link";
import styles from "../styles/geneExpViz.module.css";
import Header from "../components/header";
import Footer from "../components/footer";

const GeneExpVizPage = () => {
  const visualizationOptions = [
    {
      href: "/efp_browser",
      icon: "dna",
      title: "Electronic Fluorescent Pictograph (eFP)",
      description:
        "Tissue-specific gene expression mapping with detailed visualization",
      gradient: "gradientBioinformatics",
    },
    {
      href: "/pepperExp",
      icon: "chart-column",
      title: "Expression Heatmap",
      description: "Comprehensive multi-condition expression pattern analysis",
      gradient: "gradientData",
    },
  ];

  return (
    <div className={styles.pageWrapper}>
      <Head>
        <title>Gene Expression Visualization | PepperKB</title>
        <link
          rel="stylesheet"
          href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"
        />
        <meta
          name="description"
          content="Advanced visualization tools for pepper gene expression data analysis"
        />
      </Head>

      <Header />

      <main className={styles.mainContainer}>
        <section className={styles.heroSection}>
          <div className={styles.heroContent}>
            <h1 className={styles.heroTitle}>Gene Expression Visualization</h1>
            <p className={styles.heroSubtitle}>
              Explore comprehensive pepper gene expression datasets through
              interactive, publication-quality visualization tools
            </p>
          </div>
        </section>

        <section className={styles.visualizationSection}>
          <div className={styles.containerInner}>
            <div className={styles.toolsGrid}>
              {visualizationOptions.map((tool, index) => (
                <Link key={index} href={tool.href} className={styles.toolLink}>
                  <div
                    className={`${styles.toolCard} ${styles[tool.gradient]}`}
                  >
                    <div className={styles.toolIcon}>
                      <i className={`fa-solid fa-${tool.icon}`}></i>
                    </div>
                    <div className={styles.toolContent}>
                      <h2 className={styles.toolTitle}>{tool.title}</h2>
                      <p className={styles.toolDescription}>
                        {tool.description}
                      </p>
                    </div>
                    <div className={styles.toolAction}>
                      <span className={styles.actionText}>Explore</span>
                      <i className="fa-solid fa-arrow-right"></i>
                    </div>
                  </div>
                </Link>
              ))}
            </div>
          </div>
        </section>
      </main>

      <Footer />
    </div>
  );
};

export default GeneExpVizPage;
