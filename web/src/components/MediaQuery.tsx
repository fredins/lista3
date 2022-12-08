import MediaQuery from 'react-responsive'

type Props = {
  Mobile: JSX.Element
  Desktop: JSX.Element
}

export function DiffViews({ Mobile, Desktop }: Props,): JSX.Element {
  return (
    <MediaQuery minWidth={768} >
      {(matches: boolean) => matches
        ? Desktop
        : Mobile
      }
    </MediaQuery>
  )
}
