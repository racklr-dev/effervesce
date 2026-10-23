import agno


def run_agent(prompt):
    agent = agno.agent.Agent()
    return agent.run(prompt)
