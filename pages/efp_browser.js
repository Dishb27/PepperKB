import React, { useState } from "react";
import Head from "next/head";
import Link from "next/link";
import styles from "../styles/efpBrowser.module.css";
import Header from "../components/header";
import Footer from "../components/footer";

function EFPBrowserPage() {
  const [loadingState, setLoadingState] = useState({
    isLoading: true,
    iframeError: false,
  });

  const handleIframeLoad = () => {
    setLoadingState((prev) => ({
      ...prev,
      isLoading: false,
    }));
  };

  const handleIframeError = () => {
    setLoadingState({
      isLoading: false,
      iframeError: true,
    });
  };

  return (
    <>
      <Head>
        <title>eFP Browser - PepperKB</title>
        <meta
          name="description"
          content="Explore tissue-specific gene expression in peppers using the Expression Flower Plot (eFP) Browser"
        />
        <link
          rel="stylesheet"
          href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css"
        />
      </Head>

      <Header />

      <main className={styles.appContainer}>
        <section className={styles.heroSection}>
          <div className={styles.heroContent}>
            <div className={styles.heroTextContainer}>
              <h1 className={styles.appTitle}>
                PeppereFP
              </h1>
              <p className={styles.appDescription}>
                Visualize tissue-specific gene expression patterns in peppers
              </p>
            </div>
          </div>
        </section>

        <section className={styles.contentContainer}>
          <div className={styles.card}>
            <div className={styles.iframeContainer}>
              {loadingState.isLoading && (
                <div className={styles.loadingOverlay}>
                  <div className={styles.spinner}></div>
                  <p>Loading eFP Browser...</p>
                </div>
              )}

              {loadingState.iframeError && (
                <div className={styles.errorOverlay}>
                  <i
                    className="fas fa-exclamation-triangle"
                    aria-hidden="true"
                  ></i>
                  <p>Unable to load the eFP Browser</p>
                  <a
                    href="https://dish2711.shinyapps.io/pepper-eFP/"
                    target="_blank"
                    rel="noopener noreferrer"
                    className={styles.externalLink}
                  >
                    Open in New Tab
                  </a>
                </div>
              )}

              <iframe
                src="https://dish2711.shinyapps.io/pepper-eFP/"
                className={styles.responsiveIframe}
                frameBorder="0"
                title="Pepper eFP Browser"
                aria-label="Interactive tissue-specific gene expression visualization"
                allowFullScreen
                sandbox="allow-scripts allow-same-origin allow-forms"
                onLoad={handleIframeLoad}
                onError={handleIframeError}
              />
            </div>
          </div>
        </section>

        <nav className={styles.navigationActions}>
          <Link
            href="/geneExpViz"
            className={styles.backLink}
            aria-label="Return to Gene Expression Visualization"
          >
            <i className="fas fa-arrow-left" aria-hidden="true"></i> Back to
            Gene Expression Visualization
          </Link>
        </nav>
      </main>

      <Footer />
    </>
  );
}

export default EFPBrowserPage;
