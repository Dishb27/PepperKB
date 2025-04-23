import React, { useState } from "react";
import Head from "next/head";
import styles from "../styles/pepperClust.module.css";
import Header from "../components/header";
import Footer from "../components/footer";

const PepperClustPage = () => {
  const [isLoading, setIsLoading] = useState(true);

  const handleIframeLoad = () => {
    setIsLoading(false);
  };

  return (
    <>
      <Head>
        <title>PepperClust - PepperKB</title>
        <link
          rel="stylesheet"
          href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css"
        />
      </Head>
      <Header />
      <main className={styles.appContainer}>
        <div className={styles.heroSection}>
          <h1 className={styles.appTitle}>PepperClust</h1>
          <p className={styles.appDescription}>
            Explore gene clustering with interactive heatmaps and dendrograms.{" "}
          </p>
        </div>
        <div className={styles.contentContainer}>
          <div className={styles.card}>
            <div className={styles.iframeContainer}>
              {isLoading && (
                <div className={styles.loadingOverlay}>
                  <div className={styles.spinner}></div>
                  <p className={styles.loadingText}>
                    Loading PepperClust App...
                  </p>
                </div>
              )}
              <iframe
                src="https://dish2711.shinyapps.io/PepClust/"
                width="100%"
                height="800px"
                frameBorder="0"
                title="Pepper Gene Clustering"
                onLoad={handleIframeLoad}
                style={{ display: isLoading ? "none" : "block" }}
              />
            </div>
          </div>
        </div>
      </main>
      <Footer />
    </>
  );
};

export default PepperClustPage;
