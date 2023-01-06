import { createRoot } from 'react-dom/client';
import { StrictMode } from 'react'
import './index.css';
import reportWebVitals from './reportWebVitals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { AuthProvider } from './components/Auth'
import Home from './components/Home';
import { ActiveListProvider } from './components/useActiveList';
import { SidebarProvider } from './components/useSidebar';
import { register } from './serviceWorkerRegistration';

const queryClient = new QueryClient()

createRoot(document.getElementById('root')!).render(
  <StrictMode>
  <QueryClientProvider client={queryClient}>
  <AuthProvider>
  <ActiveListProvider>
  <SidebarProvider>
  <Home />
  </SidebarProvider>
  </ActiveListProvider>
  </AuthProvider>
  </QueryClientProvider>
  </StrictMode>
)

// Register service worker
register()

// If you want to start measuring performance in your app, pass a function
// to log results (for example: reportWebVitals(console.log))
// or send to an analytics endpoint. Learn more: https://bit.ly/CRA-vitals
reportWebVitals();
