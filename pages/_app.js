import React from "react";
import PropTypes from "prop-types";
import "@/styles/globals.css";
import "bootstrap/dist/css/bootstrap.min.css";

export default function App({ Component, pageProps }) {
  return <Component {...pageProps} />;
}

App.propTypes = {
  Component: PropTypes.elementType.isRequired,
  pageProps: PropTypes.object.isRequired,
};
