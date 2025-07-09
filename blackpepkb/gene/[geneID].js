import { useState, useEffect } from "react";
import styles from "../../styles/genes_structure.module.css";
import GeneStructure from "../../components/GeneStructure";

// Modal component for gene structure popup
const GeneStructurePopup = ({ geneId, geneData, onClose }) => {
  const [gene, setGene] = useState(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);

  useEffect(() => {
    if (!geneId) {
      setError("Gene ID not provided.");
      setLoading(false);
      return;
    }

    try {
      // Find the gene matching the geneId from the passed geneData
      const selectedGene = geneData.find((gene) => gene.GeneID === geneId);

      if (selectedGene) {
        console.log("Matched Gene Data:", selectedGene);
        setGene(selectedGene);
      } else {
        console.error(`Gene with ID ${geneId} not found.`);
        setError(`Gene with ID ${geneId} not found.`);
      }
    } catch (err) {
      console.error("Error processing gene data:", err);
      setError(err.message);
    } finally {
      setLoading(false);
    }
  }, [geneId, geneData]);

  const renderGeneStructure = () => {
    if (!gene) return null;

    // Parse gene range from the database format
    const geneRange = gene.GeneRange;

    // Parse exon ranges from the database format (comma-separated ranges)
    const exonRanges = gene.ExonRanges
      ? gene.ExonRanges.split(", ").map((range) => {
          const [start, end] = range.split(":");
          return { start: parseInt(start), end: parseInt(end) };
        })
      : [];

    // Parse intron ranges from the database format (comma-separated ranges)
    const intronRanges =
      gene.IntronRanges && gene.IntronRanges !== "-"
        ? gene.IntronRanges.split(", ").map((range) => {
            const [start, end] = range.split(":");
            return { start: parseInt(start), end: parseInt(end) };
          })
        : [];

    // Convert range objects to string format for the GeneStructure component
    const exonRangesFormatted = exonRanges.map(
      (range) => `${range.start}:${range.end}`
    );

    const intronRangesFormatted = intronRanges.map(
      (range) => `${range.start}:${range.end}`
    );

    // Customize styles for gene structure
    const exonStyles = {
      fillColor: "#137369", // Example color for exon
      height: 25, // Custom height for exons
    };

    const intronStyles = {
      strokeColor: "#949494", // Example color for introns
      strokeWidth: 3, // Custom width for intron lines
    };

    const legendStyles = {
      legendClass: styles.customLegend, // Custom class for legend
      legendRectClass: styles.customLegendRect, // Custom class for legend rect
      legendTextClass: styles.customLegendText, // Custom class for legend text
    };

    return (
      <GeneStructure
        geneRange={geneRange}
        exons={exonRangesFormatted}
        introns={intronRangesFormatted}
        exonStyles={exonStyles}
        intronStyles={intronStyles}
        legendStyles={legendStyles}
      />
    );
  };

  // Stop event propagation to prevent closing when clicking inside the modal
  const handleModalClick = (e) => {
    e.stopPropagation();
  };

  return (
    <div className={styles.modalOverlay} onClick={onClose}>
      <div className={styles.modalContent} onClick={handleModalClick}>
        <button className={styles.closeButton} onClick={onClose}>
          &times;
        </button>

        {loading ? (
          <p className={styles.loading}>Loading gene data...</p>
        ) : error ? (
          <p className={styles.error}>Error: {error}</p>
        ) : !gene ? (
          <p className={styles.error}>No data found for this gene.</p>
        ) : (
          <div className={styles.popupContent}>
            <div className={styles.geneInfoCard}>
              <h3 className={styles.geneId}>Gene ID: {gene.GeneID}</h3>
              <h4 className={styles.subHeading}>
                Number of Exons:{" "}
                <span className={styles.boldText}>
                  {gene.NumberOfExons || 0}
                </span>
              </h4>
              <h4 className={styles.subHeading}>
                Number of Introns:{" "}
                <span className={styles.boldText}>
                  {gene.NumberOfIntrons || 0}
                </span>
              </h4>
            </div>

            <div className={styles.visualizationCard}>
              <h4 className={styles.subHeading}>
                Gene Structure Visualization:
              </h4>
              {renderGeneStructure()}
            </div>
          </div>
        )}
      </div>
    </div>
  );
};

export default GeneStructurePopup;
