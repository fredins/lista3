import { RefObject, useCallback, useEffect } from 'react'

export default function useOnClickOutside<T extends HTMLElement = HTMLElement>(
  ref: RefObject<T>,
  handler: (_: MouseEvent) => void,
): void {
  const callback = useCallback((event: MouseEvent) => {
    // Ignore clicks inside element 
  const element = ref?.current
    if (!element || element.contains(event.target as Node)) return
    handler(event)
  }, [ref, handler])

  useEffect(() => {
         document.addEventListener('click', callback, true);
         return () => {
             document.removeEventListener('click', callback, true);
         };
  }, [callback]);
}
