import fs from "fs";
import path from "path";

export default function handler(req, res) {
  try {
    // Define the JSON file path
    const filePath = path.join(
      "D:",
      "Projects_nextjs",
      "pepper-website",
      "public",
      "GOtable.json"
    );

    // Check if file exists before reading
    if (!fs.existsSync(filePath)) {
      return res.status(404).json({ error: "GOtable.json not found" });
    }

    // Read the JSON file synchronously
    const data = fs.readFileSync(filePath, "utf-8");
    const records = JSON.parse(data); // Parse JSON into JavaScript objects

    // Log the records to check the contents
    console.log("Loaded GOtable.json:", records);

    // Check if JSON is empty
    if (!records || records.length === 0) {
      return res
        .status(400)
        .json({ error: "GOtable.json is empty or malformed" });
    }

    // Check if a specific Gene ID is requested (e.g., `/api/gotable?geneId=Pn1.5`)
    const { geneId } = req.query;
    console.log("Requested Gene ID:", geneId); // Log the requested Gene ID

    if (geneId) {
      // Normalize the Gene ID from query to lower case
      const normalizedGeneId = geneId.toLowerCase();

      // Filter records based on the Gene Id (also normalized to lower case)
      const filteredRecords = records.filter(
        (record) => record["Gene Id"].toLowerCase() === normalizedGeneId
      );

      // Log the filtered records for debugging
      console.log("Filtered Records for Gene ID:", filteredRecords);

      if (filteredRecords.length === 0) {
        return res
          .status(200)
          .json({ message: "No annotations available for this Gene ID" });
      }

      return res.status(200).json(filteredRecords);
    }

    // Return all records if no geneId is provided
    res.status(200).json(records);
  } catch (error) {
    console.error("Error loading GOtable.json:", error);
    res.status(500).json({ error: "Failed to load GOtable.json" });
  }
}
