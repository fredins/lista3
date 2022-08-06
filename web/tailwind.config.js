module.exports = {
  mode: 'jit',
  content: [
    "./src/**/*.{js,jsx,ts,tsx}",
  ],
  theme: {
    extend: {
      boxShadow: {
        'inner-eq': 'inset 0 0 4px 0 rgb(0 0 0 / 0.05)',
      },
    },
  },
  plugins: [],
}
