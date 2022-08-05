module.exports = {
  mode: 'jit',
  content: [
    "./src/**/*.{js,jsx,ts,tsx}",
  ],
  theme: {
    extend: {
      colors: {
        lemon: {
          50: '#FFFEE6',
          100: '#FFFCC6',
          200: '#FFFAA8',
          300: '#FFF784',
          400: '#FFF568',
          500: '#FFEF43',
        },
      },
      boxShadow: {
        'inner-eq': 'inset 0 0 4px 0 rgb(0 0 0 / 0.05)',
      },
    },
  },
  plugins: [],
}
