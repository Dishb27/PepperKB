import React, { useState, useEffect, useRef } from "react";
import Head from "next/head";
import styles from "../styles/pepperClust.module.css";
import Header from "../components/header";
import Footer from "../components/footer";

const PepperClustPage = () => {
  const [isLoading, setIsLoading] = useState(true);
  const [isMobile, setIsMobile] = useState(false);
  const iframeRef = useRef(null);

  useEffect(() => {
    const checkMobile = () => {
      setIsMobile(window.innerWidth <= 768);
    };

    checkMobile();
    window.addEventListener("resize", checkMobile);

    return () => window.removeEventListener("resize", checkMobile);
  }, []);

  const handleIframeLoad = () => {
    setIsLoading(false);

    // Try to communicate with the iframe about viewport size
    if (iframeRef.current) {
      try {
        // Send viewport information to the Shiny app
        const message = {
          type: "viewport-update",
          width: window.innerWidth,
          height: window.innerHeight,
          isMobile: window.innerWidth <= 768,
        };

        iframeRef.current.contentWindow?.postMessage(message, "*");
      } catch (e) {
        console.log("Could not communicate with iframe (CORS restriction)", e);
      }
    }
  };

  const openInNewTab = () => {
    window.open("https://dish2711.shinyapps.io/BlackPepperClust/", "_blank");
  };

  return (
    <>
      <Head>
        <title>PepperClust - PepperKB</title>
        <link
          rel="stylesheet"
          href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css"
        />
        <meta name="viewport" content="width=device-width, initial-scale=1.0" />
      </Head>
      <Header />
      <main className={styles.appContainer}>
        <div className={styles.heroSection}>
          <h1 className={styles.appTitle}>PepperClust</h1>
          <p className={styles.appDescription}>
            Explore gene clustering using interactive heatmaps and dendrogram
          </p>

          {/* Mobile warning and external link */}
          {isMobile && (
            <div className={styles.mobileNotice}>
              <p className={styles.noticeText}>
                <i className="fas fa-mobile-alt"></i>
                For the best mobile experience, we recommend opening the app in
                a new tab
              </p>
              <button className={styles.openExternalBtn} onClick={openInNewTab}>
                <i className="fas fa-external-link-alt"></i>
                Open in New Tab
              </button>
            </div>
          )}
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
                ref={iframeRef}
                src="https://dish2711.shinyapps.io/BlackPepperClust/"
                width="100%"
                height="800px"
                frameBorder="0"
                title="Pepper Gene Clustering"
                onLoad={handleIframeLoad}
                style={{
                  display: isLoading ? "none" : "block",
                  minHeight: isMobile ? "600px" : "800px",
                }}
                // Enable scrolling on mobile
                scrolling={isMobile ? "yes" : "auto"}
                // Add sandbox permissions for better interaction
                sandbox="allow-same-origin allow-scripts allow-forms allow-popups allow-pointer-lock"
              />
            </div>

            {/* Alternative mobile view */}
            {isMobile && (
              <div className={styles.mobileAlternative}>
                <p className={styles.alternativeText}>
                  Having trouble with the embedded app?
                  <button className={styles.linkButton} onClick={openInNewTab}>
                    Click here to open it directly
                  </button>
                </p>
              </div>
            )}
          </div>
        </div>
      </main>
      <Footer />
    </>
  );
};

export default PepperClustPage;
