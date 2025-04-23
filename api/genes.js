// pages/api/genes.js
import pool from "../../../lib/db";

export default async function handler(req, res) {
  const { family } = req.query; // Get family name from query parameters

  // Validate input
  if (!family || typeof family !== "string") {
    return res.status(400).json({ message: "Invalid family name provided." });
  }

  try {
    // Get a connection from the pool
    const connection = await pool.getConnection();

    // Execute the query to retrieve GeneID and additional data
    const [rows] = await connection.execute(
      `SELECT g.GeneID, g.Chromosome, g.GeneRange, g.NumberOfExons, 
              g.ExonRanges, g.NumberOfIntrons, g.IntronRanges
       FROM tf_family tf
       JOIN gene g ON tf.GeneID = g.GeneID  -- Join with genes table
       WHERE tf.TF_Family = ?`, // Filter by TF_Family
      [family]
    );

    console.log("Query result:", rows); // Debugging

    // Release the connection back to the pool
    connection.release();

    // Check if any rows were returned
    if (rows.length === 0) {
      return res
        .status(404)
        .json({ message: `No genes found for the family: ${family}.` });
    }

    // Send the response with the retrieved rows
    res.status(200).json(rows);
  } catch (error) {
    console.error("Database Error: ", error); // Log the error for debugging
    res
      .status(500)
      .json({ message: "An error occurred while retrieving data." });
  }
}
