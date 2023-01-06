module.exports = {
  mode: 'jit',
  content: [
    "./src/**/*.{js,jsx,ts,tsx}",
  ],
  theme: {
    extend: {
      boxShadow: {
        'inner-eq-1': 'inset 0 0 4px 0 rgb(0 0 0 / 0.05)',
        'inner-eq-2': 'inset 0 0 8px 0 rgb(0 0 0 / 0.05)',
        'inner-eq-3': 'inset 0 0 12px 0 rgb(0 0 0 / 0.05)',
      },
    },
  },
  plugins: [],
}
