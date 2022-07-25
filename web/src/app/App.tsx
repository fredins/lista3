import Todo from './components/Todo'

export default function App(): JSX.Element {
  return (
    <div
      className="relative mx-4 bg-sky-200 max-w-xs"
    >
      <Todo id={1} text="sill" completed={false} marked={true} />
    </div>
  );
}

