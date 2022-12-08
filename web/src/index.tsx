import { render } from 'react-dom';
import { StrictMode } from 'react'
import './index.css';
import { Routes } from './components/Routes';
import reportWebVitals from './reportWebVitals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { RouterProvider } from '@tanstack/react-router';
import { useAuth, AuthProvider } from './components/Auth'


const queryClient = new QueryClient({
// defaultOptions: {
//     queries: {
//       staleTime: 20*1000,
//     },
//   },
 }
)

render(
  <StrictMode>
      <QueryClientProvider client={queryClient}>
       <AuthProvider>
         <Routes />
       </AuthProvider>
      </QueryClientProvider>
  </StrictMode>,
  document.getElementById('root')
);

// If you want to start measuring performance in your app, pass a function
// to log results (for example: reportWebVitals(console.log))
// or send to an analytics endpoint. Learn more: https://bit.ly/CRA-vitals
reportWebVitals();
