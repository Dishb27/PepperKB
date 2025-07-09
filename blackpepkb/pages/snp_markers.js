import React from "react";
import { Database, ChevronRight } from "lucide-react";
import { useRouter } from "next/router";
import Head from "next/head";
import Footer from "../components/footer";
import Header from "../components/header";
import styles from "../styles/snp.module.css";

const SNPMarkerPage = () => {
  const router = useRouter();

  const handleStudySelect = (studyId, id) => {
    router.push({
      pathname: "/snp_study",
      query: { study: studyId, id },
    });
  };

  const studies = [
    {
      id: "00N", // Hidden from UI but used in backend
      year: "2024",
      title: `Genetic diversity and population structure of <em>Piper nigrum</em> (black pepper) 
      accessions based on next-generation SNP markers.`,
      studyId: "study1", // Hidden from UI but used in backend
      authors: [
        "Wimalarathna N.A.",
        "Wickramasuriya A.M.",
        "Metschina D.",
        "Cauz-Santos L.A.",
        "Bandupriya D.",
        "Ariyawansa K.G.S.U.",
        "Gopallawa B.",
        "Chase M.W.",
        "Samuel R.",
        "Silva T.D.",
      ],
    },
    {
      id: "00H", // Hidden from UI but used in backend
      year: "2025",
      title: `A comprehensive catalog of single nucleotide polymorphisms (SNPs) from the black pepper (<em>Piper nigrum</em> L.) genome`,
      studyId: "study2", // Hidden from UI but used in backend
      authors: [
        "Thanthirige, H.A.",
        "Wimalarathna, N.A.",
        "Wickramasuriya, A.M.",
      ],
    },
  ];

  return (
    <>
      <Head>
        <title>SNP Marker Studies - PepperKB</title>
        <meta
          name="description"
          content="Explore our comprehensive SNP marker database for Black Pepper genomics research"
        />
      </Head>

      <div className={styles.snpMarkerPage}>
        <Header />

        <main className={styles.snpMarkerContainer}>
          <div className={styles.heroSection}>
            <div className={styles.heroContent}>
              <span className={styles.tagline}>BLACK PEPPER KNOWLEDGEBASE</span>
              <h1>SNP Marker Database</h1>
              <p>
                Explore single nucleotide polymorphism (SNP) markers curated
                from black pepper research data.
              </p>
            </div>
          </div>

          <div className={styles.contentSection}>
            <div className={styles.studyCards}>
              {studies.map((study) => (
                <div
                  key={study.id}
                  className={styles.studyCard}
                  onClick={() => handleStudySelect(study.studyId, study.id)}
                >
                  <div className={styles.studyIconWrapper}>
                    <Database size={28} strokeWidth={1.5} />
                  </div>
                  <div className={styles.studyInfo}>
                    <div className={styles.studyDetails}>
                      <div className={styles.studyDetailItem}>
                        <span className={styles.studyDetailLabel}>Year:</span>
                        <span>{study.year}</span>
                      </div>

                      <div className={styles.studyDetailItem}>
                        <span className={styles.studyDetailLabel}>Title:</span>
                        <p
                          className={styles.studyTitle}
                          dangerouslySetInnerHTML={{ __html: study.title }}
                        />
                      </div>

                      <div className={styles.studyDetailItem}>
                        <span className={styles.studyDetailLabel}>
                          Authors:
                        </span>
                        <div className={styles.tagsList}>
                          {study.authors.map((author) => (
                            <span key={author} className={styles.tag}>
                              {author}
                            </span>
                          ))}
                        </div>
                      </div>
                    </div>

                    <div className={styles.studyFooter}>
                      <span className={styles.viewStudyLink}>
                        View SNPs <ChevronRight size={16} />
                      </span>
                    </div>
                  </div>
                </div>
              ))}
            </div>
          </div>
        </main>

        <Footer />
      </div>
    </>
  );
};

export default SNPMarkerPage;
