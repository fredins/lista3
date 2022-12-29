import MediaQuery, { useMediaQuery } from 'react-responsive'

export {
  AdaptiveView,
  useDesktopMediaQuery,
  useMobileMediaQuery,
  Desktop,
  Mobile,
}

const useDesktopMediaQuery = () =>
  useMediaQuery({ query: "(min-width: 1280px)" })
const useMobileMediaQuery = () => 
  useMediaQuery({ query: "(max-width: 767px)" })

type ChildrenProp = { children : JSX.Element }

const Desktop = ({ children } : ChildrenProp ) => 
  useDesktopMediaQuery() ? children : null
const Mobile = ({ children } : ChildrenProp ) => 
  useMobileMediaQuery() ? children : null

type Props = {
  desktop: JSX.Element
  mobile: JSX.Element
}

function AdaptiveView({ desktop, mobile }: Props): JSX.Element {
  return (
    <MediaQuery minWidth={768} >
      {(matches: boolean) => matches
        ? desktop
        : mobile
      }
    </MediaQuery>
  )
}
