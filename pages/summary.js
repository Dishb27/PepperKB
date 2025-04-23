import React from "react";
import { useRouter } from "next/router";
import { useState, useEffect } from "react";
import {
  FaSpinner,
  FaExclamationCircle,
  FaDna,
  FaLayerGroup,
  FaSitemap,
  FaThLarge,
  FaProjectDiagram,
  FaInfoCircle,
  FaFileCode,
  FaStream,
} from "react-icons/fa";
import { GiChromosome } from "react-icons/gi";
import Footer from "../components/footer";
import styles from "../styles/summary.module.css";
import Header from "../components/header";

const SummaryPage = () => {
  const router = useRouter();
  const { geneID } = router.query;
  const [geneData, setGeneData] = useState(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);
  const [activeTab, setActiveTab] = useState("geneDetails");
  const [isMenuOpen, setIsMenuOpen] = useState(false); // State for mobile menu
  const [openDropdown, setOpenDropdown] = useState(null); // State for dropdown

  const [goTableData, setGoTableData] = useState([]);

  useEffect(() => {
    if (geneID) {
      const fetchGoTableData = async () => {
        try {
          const response = await fetch(
            `/api/gene-data?action=getGoTable&geneId=${geneID}`
          );
          if (!response.ok) throw new Error("Failed to load GO table");
          const data = await response.json();
          console.log("GO Table Data from API:", data); // Log the GO table data
          setGoTableData(data);
        } catch (error) {
          console.error(error);
        }
      };

      fetchGoTableData();
    }
  }, [geneID]); // Only run this effect if geneID changes

  // Toggle mobile menu
  const toggleMenu = () => setIsMenuOpen(!isMenuOpen);

  // Toggle dropdown
  const toggleDropdown = (index) =>
    setOpenDropdown(openDropdown === index ? null : index);

  useEffect(() => {
    if (!geneID) return;

    const fetchGeneData = async () => {
      try {
        const response = await fetch(
          `/api/gene-data?action=getGeneById&geneId=${geneID}`
        );
        if (!response.ok) {
          throw new Error("Gene data not found");
        }
        const data = await response.json();

        // Find the first matching GO entry for CDS and Protein Sequences
        const goEntry = goTableData.find(
          (row) =>
            row["Gene Id"].trim().toLowerCase() === geneID.trim().toLowerCase()
        );

        setGeneData({
          ...data,
          "CDS Sequence": goEntry?.["CDS Sequence"] || "N/A",
          "Protein Sequence": goEntry?.["Protein Sequence"] || "N/A",
        });

        setLoading(false);
      } catch (err) {
        setError(err.message);
        setLoading(false);
      }
    };

    fetchGeneData();
  }, [geneID, goTableData]); // Re-run if geneID or GO table data changes

  const tabConfig = {
    geneDetails: {
      icon: <FaInfoCircle />,
      label: "Gene Details",
    },
    exploreChromosome: {
      icon: <FaStream />,
      label: "Coding Sequence (CDS)",
    },
    geneFamily: {
      icon: <FaLayerGroup />,
      label: "Protein Sequence",
    },
    goTable: {
      icon: <FaThLarge />,
      label: "Gene Ontology",
    },
  };

  const renderTabContent = () => {
    if (activeTab === "geneDetails") {
      return (
        <section className={styles.tabContent}>
          <ul className={styles.cardList}>
            <li className={styles.card}>
              <div className={styles.cardContent}>
                <strong>Chromosome ID:</strong>
                <span>{geneData.Chromosome_ID || "N/A"}</span>
              </div>
            </li>
            <li className={styles.card}>
              <div className={styles.cardContent}>
                <strong>TF Family:</strong>
                <span>{geneData.TF_Family || "N/A"}</span>
              </div>
            </li>
            <li className={styles.card}>
              <div className={styles.cardContent}>
                <strong>Gene Range:</strong>
                <span>{geneData.Gene_Range || "N/A"}</span>
              </div>
            </li>
          </ul>
        </section>
      );
    } else if (activeTab === "exploreChromosome") {
      return (
        <section className={styles.tabContent}>
          <div className={styles.sequenceContainer}>
            <div className={styles["sequence-wrapper"]}>
              <p className={styles.sequence}>
                {geneData["CDS Sequence"] || "N/A"}
              </p>
            </div>
          </div>
        </section>
      );
    } else if (activeTab === "geneFamily") {
      return (
        <section className={styles.tabContent}>
          <div className={styles.sequenceContainer}>
            <div className={styles["sequence-wrapper"]}>
              <p className={styles.sequence}>
                {geneData["Protein Sequence"] || "N/A"}
              </p>
            </div>
          </div>
        </section>
      );
    } else if (activeTab === "goTable") {
      console.log("GO Table Data:", goTableData);

      // Ensure goTableData is an array before calling filter
      const filteredGoData = Array.isArray(goTableData)
        ? goTableData.filter(
            (row) =>
              row["Gene Id"].trim().toLowerCase() ===
              geneID.trim().toLowerCase()
          )
        : [];

      console.log("Filtered GO Data for Gene ID", geneID, ":", filteredGoData);

      return (
        <section className={styles.tabContent}>
          {/* Legend for GO Types */}
          <div className={styles.legend}>
            <ul>
              <li>
                <span className={styles.F}>F</span> - Molecular Function
              </li>
              <li>
                <span className={styles.P}>P</span> - Biological Process
              </li>
              <li>
                <span className={styles.C}>C</span> - Cellular Component
              </li>
            </ul>
          </div>

          {filteredGoData.length === 0 ? (
            <p className={styles.noData}>
              No GO annotations available for {geneID}.
            </p>
          ) : (
            <div className={styles.tableContainer}>
              <table className={styles.goTable}>
                <thead>
                  <tr>
                    <th>GO Term</th>
                    <th>GO ID</th>
                    <th>Type</th>
                  </tr>
                </thead>
                <tbody>
                  {filteredGoData.map((row, index) => (
                    <tr key={index}>
                      <td>{row["GO Term"]}</td>
                      <td>{row["GO Id"]}</td>
                      <td className={styles[row["Type"]]}>{row["Type"]}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          )}
        </section>
      );
    }
  };

  return (
    <>
      {/* Header Section */}
      <Header />

      {/* Main Content */}
      <div className={styles.container}>
        <section className={styles.geneIDSection}>
          <h2>{geneID || "Not Provided"}</h2>
          <p>
            {geneID
              ? `Displaying details for Gene ID: ${geneID}`
              : "Please provide a Gene ID to view its summary."}
          </p>
        </section>

        {loading ? (
          <div className={styles.loading}>
            <FaSpinner className={styles.spinner} />
            <p>Loading gene data...</p>
          </div>
        ) : error ? (
          <div className={styles.error}>
            <FaExclamationCircle className={styles.errorIcon} />
            <p>{error}</p>
          </div>
        ) : (
          geneData && (
            <div className={styles.tabbedInterface}>
              <div className={styles.tabs}>
                {Object.entries(tabConfig).map(([key, { icon, label }]) => (
                  <button
                    key={key}
                    className={`${styles.tabButton} ${
                      activeTab === key ? styles.activeTab : ""
                    }`}
                    onClick={() => setActiveTab(key)}
                  >
                    <span className={styles.tabIcon}>{icon}</span>
                    {label}
                  </button>
                ))}
              </div>
              {renderTabContent()}
            </div>
          )
        )}
      </div>
      <Footer />
    </>
  );
};

export default SummaryPage;
