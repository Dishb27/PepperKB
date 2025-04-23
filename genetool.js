import React, { useState } from "react";
import Head from "next/head";
import GeneStructure from "../components/GeneStructure";
import styles from "../styles/genetool.module.css";
import Header from "../components/header";
import Footer from "../components/footer";
const MAX_GENES = 5;

const GeneToolPage = () => {
  const [geneIds, setGeneIds] = useState("");
  const [geneDetails, setGeneDetails] = useState([]);
  const [error, setError] = useState("");
  const [invalidGeneIds, setInvalidGeneIds] = useState([]);
  const [isLoading, setIsLoading] = useState(false);

  const fetchGeneData = async (ids) => {
    setIsLoading(true);
    try {
      // Make API call to fetch gene data
      const response = await fetch("/api/genes-by-id", {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify({ geneIds: ids }),
      });

      if (!response.ok) {
        throw new Error(`API error: ${response.status}`);
      }

      const fetchedGenes = await response.json();

      // Check which IDs were not found
      const fetchedIds = fetchedGenes.map((gene) => gene.GeneID.toUpperCase());
      const notFoundIds = ids.filter(
        (id) => !fetchedIds.includes(id.toUpperCase())
      );

      setInvalidGeneIds(notFoundIds);

      // Transform fetched data to the format expected by the component
      const formattedGenes = fetchedGenes.map((gene) => {
        // Parse exon ranges from the database format (comma-separated ranges)
        const exonRanges = gene.ExonRange
          ? gene.ExonRange.split(", ").map((range) => range)
          : [];

        // Parse intron ranges from the database format (comma-separated ranges)
        const intronRanges =
          gene.IntronRange && gene.IntronRange !== "-"
            ? gene.IntronRange.split(", ").map((range) => range)
            : [];

        return {
          id: gene.GeneID,
          geneRange: gene.GeneRange,
          exons: exonRanges,
          introns: intronRanges,
          numberOfExons: gene.NumberOfExons || 0,
          numberOfIntrons: gene.NumberOfIntrons || 0,
        };
      });

      setGeneDetails(formattedGenes);

      if (formattedGenes.length === 0) {
        setError("No valid GeneIDs found.");
      } else {
        setError("");
      }
    } catch (err) {
      console.error("Error fetching gene data:", err);
      setError("Failed to fetch gene data. Please try again.");
    } finally {
      setIsLoading(false);
    }
  };

  const handleSubmit = (e) => {
    e.preventDefault();
    setError("");
    setInvalidGeneIds([]);

    const ids = geneIds
      .toUpperCase()
      .split(/[,\n]+/)
      .map((id) => id.trim())
      .filter((id) => id);

    if (ids.length === 0) {
      setError("Please enter at least one GeneID.");
      return;
    }

    if (ids.length > MAX_GENES) {
      setError(`You can enter a maximum of ${MAX_GENES} GeneIDs.`);
      return;
    }

    fetchGeneData(ids);
  };

  const handleClear = () => {
    setGeneIds("");
    setGeneDetails([]);
    setError("");
    setInvalidGeneIds([]);
  };

  const handleExample = () => {
    setGeneIds("Pn1.2085, Pn1.102, Pn10.1094, Pn11.2697, Pn15.2213");
  };

  // const toggleMenu = () => setIsMenuOpen(!isMenuOpen);
  // const toggleDropdown = (index) =>
  //   setOpenDropdown(openDropdown === index ? null : index);

  // Customize styles for gene structure
  const exonStyles = {
    fillColor: "#137369", // Custom color for exon
    height: 25, // Custom height for exons
  };

  const intronStyles = {
    strokeColor: "#949494", // Custom color for introns
    strokeWidth: 3, // Custom width for intron lines
  };

  return (
    <>
      <Head>
        <title>GeneViz - PepperKB</title>
        <link
          rel="stylesheet"
          href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css"
        />
      </Head>

      <Header />
      <div className={styles.container}>
        <h1 className={styles.title}>GeneViz</h1>
        <p className={styles.subtitle}>
          Visualize the structure of your gene of interest with ease. Input
          GeneIDs to generate detailed exon-intron maps.
        </p>

        <div className={styles.formCard}>
          <button
            type="button"
            onClick={handleExample}
            className={styles.exampleBtn}
          >
            Try an Example
          </button>

          <form onSubmit={handleSubmit}>
            <textarea
              value={geneIds}
              onChange={(e) => setGeneIds(e.target.value)}
              placeholder={`Enter up to ${MAX_GENES} GeneIDs, separated by commas or new lines`}
              rows={5}
              className={styles.textarea}
            />
            <div className={styles.buttons}>
              <button
                type="submit"
                className={styles.submitBtn}
                disabled={isLoading}
              >
                {isLoading ? "Processing..." : "Submit"}
              </button>
              <button
                type="button"
                className={styles.clearBtn}
                onClick={handleClear}
              >
                Clear
              </button>
            </div>
          </form>

          {error && <div className={styles.error}>{error}</div>}

          {invalidGeneIds.length > 0 && (
            <div className={styles.warning}>
              The following GeneIDs were not found: {invalidGeneIds.join(", ")}
            </div>
          )}
        </div>

        <div className={styles.results}>
          {geneDetails.map((gene) =>
            gene ? (
              <div key={gene.id} className={styles.geneCard}>
                <h3 className={styles.geneTitle}>{gene.id}</h3>
                <div className={styles.geneInfoCard}>
                  <h4 className={styles.subHeading}>
                    Number of Exons:{" "}
                    <span className={styles.boldText}>
                      {gene.numberOfExons}
                    </span>
                  </h4>
                  <h4 className={styles.subHeading}>
                    Number of Introns:{" "}
                    <span className={styles.boldText}>
                      {gene.numberOfIntrons}
                    </span>
                  </h4>
                </div>
                {gene.geneRange && gene.exons && gene.introns ? (
                  <div className={styles.geneContent}>
                    <GeneStructure
                      geneRange={gene.geneRange}
                      exons={gene.exons}
                      introns={gene.introns}
                      exonStyles={exonStyles}
                      intronStyles={intronStyles}
                    />
                  </div>
                ) : (
                  <p className={styles.error}>
                    Data not available for {gene.id}
                  </p>
                )}
              </div>
            ) : null
          )}
        </div>
      </div>
      <Footer />
    </>
  );
};

export default GeneToolPage;
