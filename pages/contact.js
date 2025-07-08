import React, { useState } from "react";
import {
  User,
  Mail,
  MessageSquare,
  ArrowRight,
  CheckCircle,
} from "lucide-react";
import Footer from "../components/footer";
import Header from "../components/header";
import styles from "../styles/contact.module.css";

const ContactForm = () => {
  const [formData, setFormData] = useState({
    firstName: "",
    lastName: "",
    email: "",
    message: "",
  });

  const [focused, setFocused] = useState({
    firstName: false,
    lastName: false,
    email: false,
    message: false,
  });

  const [submitted, setSubmitted] = useState(false);
  const [isSubmitting, setIsSubmitting] = useState(false);
  const [error, setError] = useState(null);

  const handleChange = (e) => {
    const { name, value } = e.target;
    setFormData((prev) => ({
      ...prev,
      [name]: value,
    }));
  };

  const handleFocus = (name) => {
    setFocused((prev) => ({
      ...prev,
      [name]: true,
    }));
  };

  const handleBlur = (name) => {
    if (!formData[name]) {
      setFocused((prev) => ({
        ...prev,
        [name]: false,
      }));
    }
  };

  const handleSubmit = async (e) => {
    e.preventDefault();
    setIsSubmitting(true);
    setError(null);

    try {
      const response = await fetch("/api/contact", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(formData),
      });

      if (response.ok) {
        setSubmitted(true);
        setTimeout(() => {
          setFormData({
            firstName: "",
            lastName: "",
            email: "",
            message: "",
          });
          setFocused({
            firstName: false,
            lastName: false,
            email: false,
            message: false,
          });
          setSubmitted(false);
        }, 3000);
      } else {
        const errorData = await response.json();
        setError(
          errorData.message || "Failed to send message. Please try again."
        );
      }
    } catch {
      setError(
        "An error occurred while sending your message. Please try again."
      );
    } finally {
      setIsSubmitting(false);
    }
  };

  return (
    <div className={styles.contactPage}>
      <div className={styles.backgroundPattern}></div>
      <Header />

      <main className={styles.contactContainer}>
        <div className={styles.contactCard}>
          <div className={styles.accentBar}></div>

          <div className={styles.cardContent}>
            <div className={styles.headerSection}>
              <span className={styles.tagline}>BLACK PEPPER KNOWLEDGEBASE</span>
              <h1>GET IN TOUCH</h1>
              <p>
                If you have any questions, comments, or inquiries, please feel
                free to contact us. We are here to assist you with any issues
                you encounter while browsing the BlackPepKB.
              </p>
            </div>

            <form
              className={`${styles.contactForm} ${
                submitted ? styles.submitted : ""
              } ${error ? styles.hasError : ""}`}
              onSubmit={handleSubmit}
            >
              {submitted ? (
                <div className={styles.successMessage}>
                  <div className={styles.successIconWrapper}>
                    <CheckCircle size={48} strokeWidth={1.5} />
                  </div>
                  <h2>Message Sent!</h2>
                  <p>
                    Thank you for reaching out. We&apos;ll get back to you soon.
                  </p>
                </div>
              ) : (
                <div className={styles.formContent}>
                  {error && (
                    <div className={styles.errorMessage}>
                      <p>{error}</p>
                    </div>
                  )}

                  <div className={styles.formRow}>
                    <div className={styles.formGroup}>
                      <div className={styles.inputIcon}>
                        <User size={18} />
                      </div>
                      <input
                        type="text"
                        name="firstName"
                        id="firstName"
                        value={formData.firstName}
                        onChange={handleChange}
                        onFocus={() => handleFocus("firstName")}
                        onBlur={() => handleBlur("firstName")}
                        required
                      />
                      <label
                        htmlFor="firstName"
                        className={
                          focused.firstName || formData.firstName
                            ? styles.active
                            : ""
                        }
                      >
                        First Name
                      </label>
                      <div className={styles.focusBorder}></div>
                    </div>

                    <div className={styles.formGroup}>
                      <div className={styles.inputIcon}>
                        <User size={18} />
                      </div>
                      <input
                        type="text"
                        name="lastName"
                        id="lastName"
                        value={formData.lastName}
                        onChange={handleChange}
                        onFocus={() => handleFocus("lastName")}
                        onBlur={() => handleBlur("lastName")}
                        required
                      />
                      <label
                        htmlFor="lastName"
                        className={
                          focused.lastName || formData.lastName
                            ? styles.active
                            : ""
                        }
                      >
                        Last Name
                      </label>
                      <div className={styles.focusBorder}></div>
                    </div>
                  </div>

                  <div className={styles.formGroup}>
                    <div className={styles.inputIcon}>
                      <Mail size={18} />
                    </div>
                    <input
                      type="email"
                      name="email"
                      id="email"
                      value={formData.email}
                      onChange={handleChange}
                      onFocus={() => handleFocus("email")}
                      onBlur={() => handleBlur("email")}
                      required
                    />
                    <label
                      htmlFor="email"
                      className={
                        focused.email || formData.email ? styles.active : ""
                      }
                    >
                      Email Address
                    </label>
                    <div className={styles.focusBorder}></div>
                  </div>

                  <div className={styles.formGroup}>
                    <div
                      className={`${styles.inputIcon} ${styles.messageIcon}`}
                    >
                      <MessageSquare size={18} />
                    </div>
                    <textarea
                      name="message"
                      id="message"
                      value={formData.message}
                      onChange={handleChange}
                      onFocus={() => handleFocus("message")}
                      onBlur={() => handleBlur("message")}
                      required
                    ></textarea>
                    <label
                      htmlFor="message"
                      className={
                        focused.message || formData.message ? styles.active : ""
                      }
                    >
                      Your Message
                    </label>
                    <div className={styles.focusBorder}></div>
                  </div>

                  <button
                    type="submit"
                    className={styles.submitButton}
                    disabled={isSubmitting}
                  >
                    {isSubmitting ? (
                      <span className={styles.loadingSpinner}></span>
                    ) : (
                      <>
                        <span>Send Message</span>
                        <ArrowRight size={18} />
                      </>
                    )}
                  </button>
                </div>
              )}
            </form>
          </div>

          <div className={styles.contactDecoration}>
            <div className={styles.decorElement1}></div>
            <div className={styles.decorElement2}></div>
            <div className={styles.decorElement3}></div>
            <div className={styles.decorElement4}></div>
          </div>
        </div>
      </main>

      <Footer />
    </div>
  );
};

export default ContactForm;
