module.exports = {
  root: "wwwroot",
  build: {
    rollupOptions: {
      input: [
        "./Scripts/MemoryGameSPA.min.js",
        "./Scripts/MemoryGameSPA.css"
      ]
    }
  }
}
