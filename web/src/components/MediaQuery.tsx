import MediaQuery, { useMediaQuery } from 'react-responsive'

const desktopMinWidth = 768
const mobileMaxWidth = desktopMinWidth - 1

const useDesktopMediaQuery = () =>
  useMediaQuery({ minWidth: desktopMinWidth })
const useMobileMediaQuery = () => 
  useMediaQuery({ maxWidth: mobileMaxWidth })

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
    <MediaQuery minWidth={desktopMinWidth} >
      {(matches: boolean) => matches
        ? desktop
        : mobile
      }
    </MediaQuery>
  )
}

export {
  AdaptiveView,
  useDesktopMediaQuery,
  useMobileMediaQuery,
  Desktop,
  Mobile,
}
