import { render } from 'react-dom';
import { StrictMode } from 'react'
import './index.css';
import reportWebVitals from './reportWebVitals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { AuthProvider } from './components/Auth'
import Home from './components/Home';
import { ActiveListProvider } from './components/useActiveList';
import { ReactQueryDevtools } from '@tanstack/react-query-devtools'

const queryClient = new QueryClient()

render(
  <StrictMode>
  <QueryClientProvider client={queryClient}>
  <AuthProvider>
  <ActiveListProvider>
  <Home />
  </ActiveListProvider>
  </AuthProvider>
  <ReactQueryDevtools />
  </QueryClientProvider>
  </StrictMode>,
  document.getElementById('root')
);

// If you want to start measuring performance in your app, pass a function
// to log results (for example: reportWebVitals(console.log))
// or send to an analytics endpoint. Learn more: https://bit.ly/CRA-vitals
reportWebVitals();
